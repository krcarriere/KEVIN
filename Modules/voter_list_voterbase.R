# ══════════════════════════════════════════════════════════════════════════
# voter_list_voterbase.R
#
# "Voter Base" mode of the Voter List tab — full role-scope filtering with
# party, support, and "known by" filters, plus toggleable display columns.
#
# Performance design:
#   - filtered_display() reactive does the expensive joins + filters once
#     per filter change, caches the result.
#   - renderDT just slices columns + paginates — cheap.
#   - visible_n cap means even huge universes only push 200 rows at a time
#     to DT; "Load more" button bumps the cap.
# ══════════════════════════════════════════════════════════════════════════

voterListVoterBaseFiltersUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::accordion(
    id = ns("filter_accordion"),
    open = c("Party", "Support Rating"),
    multiple = TRUE,
    
    bslib::accordion_panel(
      "Party",
      icon = bsicons::bs_icon("flag"),
      shiny::checkboxGroupInput(
        ns("party"), NULL,
        choices = c("Democrat"     = "D",
                    "Unaffiliated" = "U",
                    "Republican"   = "R",
                    "Unknown"      = "NA"),
        selected = c("D", "U", "R", "NA")
      )
    ),
    
    bslib::accordion_panel(
      "Support Rating",
      icon = bsicons::bs_icon("hand-thumbs-up"),
      shiny::checkboxGroupInput(
        ns("support"), NULL,
        # "3" (no opinion) and unrated/"unknown" voters are intentionally
        # never offered here — Voter Base view is restricted to voters
        # with a clear lean either way. See filtered_display() below,
        # which also hard-enforces this regardless of input$support.
        choices = c("1 \u2013 Strong Support"         = "1",
                    "2 \u2013 Lean Support"           = "2",
                    "4 \u2013 Und / Lean Oppose"      = "4",
                    "5 \u2013 Lean Oppose"            = "5"),
        selected = c("1", "2", "4", "5")
      )
    ),
    
    bslib::accordion_panel(
      "Known By",
      icon = bsicons::bs_icon("person-check"),
      shiny::checkboxGroupInput(ns("who_knows"), NULL,
                                choices = c(), selected = NULL),
      shiny::checkboxInput(ns("who_knows_unknown"),
                           "Include voters no one knows",
                           value = TRUE)
    ),
    
    bslib::accordion_panel(
      "Show columns",
      icon = bsicons::bs_icon("layout-three-columns"),
      shiny::checkboxGroupInput(
        ns("show_cols"), NULL,
        choices = c("Street"       = "Street",
                    "Precinct"     = "Precinct",
                    "Party"        = "Party",
                    "Support"      = "Support",
                    "Known By"     = "KnownBy",
                    "Last Contact" = "LastContact",
                    "Concerns"     = "Concerns"),
        selected = c("Street", "Party", "Support", "LastContact")
      )
    )
  )
}

voterListVoterBaseTableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("table")),
    shiny::uiOutput(ns("load_more_ui"))
  )
}

voterListVoterBaseServer <- function(id, con, searchable_voters,
                                     credentials, data_refresh,
                                     who_initials) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # ── Pagination state ────────────────────────────────────────────
    # Caps how many rows we ship to DT. Reset to 200 whenever a filter
    # changes so the user always starts at "first 200" of a fresh result.
    visible_n <- shiny::reactiveVal(200L)
    
    shiny::observeEvent(
      list(input$party, input$support,
           input$who_knows, input$who_knows_unknown),
      {
        visible_n(200L)
      },
      ignoreInit = TRUE
    )
    
    shiny::observeEvent(input$load_more, {
      visible_n(visible_n() + 200L)
    })
    
    # ── Populate the Known By choices dynamically ───────────────────
    shiny::observe({
      shiny::req(credentials()$user_auth)
      v <- searchable_voters()
      all_strs <- v$WhoKnows[!is.na(v$WhoKnows) & v$WhoKnows != ""]
      if (length(all_strs) == 0) {
        choices <- character(0)
      } else {
        initials <- unlist(strsplit(all_strs, ",\\s*"))
        initials <- unique(trimws(initials))
        initials <- initials[nzchar(initials)]
        initials <- sort(initials)
        initial_to_user <- stats::setNames(names(who_initials), who_initials)
        labels <- vapply(initials, function(i) {
          user <- initial_to_user[i]
          if (is.na(user)) i else paste0(user_campaigner(user), " (", i, ")")
        }, character(1))
        choices <- stats::setNames(initials, labels)
      }
      shiny::updateCheckboxGroupInput(session, "who_knows",
                                      choices = choices,
                                      selected = choices)
    })
    
    # ── Heavy compute: filter + joins (recomputes only on filter change) ──
    # Always builds the full 8-column display tibble. Column-toggle changes
    # are handled at the render layer so this reactive doesn't re-fire.
    filtered_display <- shiny::reactive({
      data_refresh()
      
      v <- searchable_voters()
      
      latest_support <- latest_field_map(con, "support_rating")
      v <- v |>
        dplyr::mutate(
          VoterId = as.character(VoterId),
          LatestSupport = unname(latest_support[VoterId]),
          LatestSupport = dplyr::if_else(
            (is.na(LatestSupport) | LatestSupport == "") &
              !is.na(is_dtc) & is_dtc == 1,
            "1", LatestSupport
          )
        )
      
      # Apply filters first — narrows the row count before expensive joins
      keep_parties <- input$party %||% character(0)
      include_na_party <- "NA" %in% keep_parties
      real_parties <- setdiff(keep_parties, "NA")
      v <- v |> dplyr::filter(
        (Party %in% real_parties) | (include_na_party & is.na(Party))
      )
      
      # Hard rule, not just a default: only 1/2/4/5 are ever eligible here,
      # regardless of what input$support contains. "3" and unrated/unknown
      # voters are never shown in Voter Base view, for anyone.
      keep_support <- intersect(input$support %||% character(0),
                                c("1", "2", "4", "5"))
      v <- v |> dplyr::filter(LatestSupport %in% keep_support)
      
      # Hard rule: only voters who've actually been knocked (have a
      # contacts row) are eligible — "not knocked" voters never show up
      # here, for anyone. DTC members are the one exception, since
      # they're already treated as pre-counted supporters everywhere
      # else in the app even without a literal door knock.
      contacted_ids <- voters_with_any_contact(con)
      v <- v |> dplyr::filter(
        VoterId %in% contacted_ids 
      )
      
      keep_initials <- input$who_knows %||% character(0)
      
      
      include_no_one <- isTRUE(input$who_knows_unknown)
      v <- v |> dplyr::filter(
        (
          !is.na(WhoKnows) & WhoKnows != "" &
            sapply(strsplit(WhoKnows, ",\\s*"), function(parts) {
              any(trimws(parts) %in% keep_initials)
            })
        ) | (
          include_no_one & (is.na(WhoKnows) | WhoKnows == "")
        )
      )
      
      if (nrow(v) == 0) return(tibble::tibble())
      
      # Joins after filters: only attach concerns/timestamps to the
      # voters who survived. Far less work than joining 22k voters then
      # filtering.
      scope_vids <- as.character(v$VoterId)
      
      latest_concerns <- dplyr::tbl(con, "contacts") |>
        dplyr::filter(voter_id %in% !!scope_vids) |>
        dplyr::select(voter_id, contact_timestamp,
                      concern1, concern1_other,
                      concern2, concern2_other,
                      concern3, concern3_other) |>
        dplyr::collect() |>
        dplyr::group_by(voter_id) |>
        dplyr::slice_max(contact_timestamp, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          Concerns = {
            resolve <- function(c, o) {
              if (is.na(c) || c == "" || c == "(none)") return(NA_character_)
              if (c == "Other" && !is.na(o) && nzchar(o)) return(o)
              c
            }
            mapply(function(c1, o1, c2, o2, c3, o3) {
              parts <- c(resolve(c1, o1), resolve(c2, o2), resolve(c3, o3))
              parts <- parts[!is.na(parts)]
              if (length(parts) == 0) NA_character_ else paste(parts, collapse = ", ")
            },
            concern1, concern1_other,
            concern2, concern2_other,
            concern3, concern3_other,
            USE.NAMES = FALSE)
          }
        ) |>
        dplyr::select(voter_id, Concerns)
      
      latest_ts <- latest_field_map(con, "contact_timestamp")
      
      v |>
        dplyr::mutate(VoterId = as.character(VoterId)) |>
        dplyr::left_join(latest_concerns, by = c("VoterId" = "voter_id")) |>
        dplyr::mutate(
          Street = display_address(Street),
          LastContact = unname(latest_ts[VoterId]),
          LastContact = dplyr::if_else(is.na(LastContact), "",
                                       substr(LastContact, 1, 10)),
          Support = dplyr::if_else(is.na(LatestSupport), "-", LatestSupport),
          KnownBy = dplyr::if_else(is.na(WhoKnows) | WhoKnows == "",
                                   "-", WhoKnows),
          Concerns = dplyr::if_else(is.na(Concerns), "", Concerns)
        ) |>
        dplyr::select(Name, Street, Precinct, Party, Support,
                      KnownBy, LastContact, Concerns) |>
        dplyr::arrange(Name)
    })
    
    # ── Light render: slice columns, paginate, send to DT ───────────
    output$table <- DT::renderDT({
      display <- filtered_display()
      
      if (nrow(display) == 0) {
        return(DT::datatable(
          tibble::tibble(Notice = "No voters match these filters."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      
      chosen_optional <- input$show_cols %||% character(0)
      selected_cols <- c("Name", chosen_optional)
      
      shown <- head(display, visible_n())
      
      display_final <- shown |>
        dplyr::select(dplyr::all_of(selected_cols)) |>
        dplyr::rename_with(~ dplyr::case_when(
          .x == "KnownBy"     ~ "Known By",
          .x == "LastContact" ~ "Last Contact",
          TRUE                ~ .x
        ))
      
      DT::datatable(display_final,
                    options = list(pageLength = 50, dom = "tip"),
                    rownames = FALSE)
    })
    
    # ── Load-more button + caption ──────────────────────────────────
    output$load_more_ui <- shiny::renderUI({
      display <- filtered_display()
      if (nrow(display) == 0) return(NULL)
      
      shown <- visible_n()
      total <- nrow(display)
      
      if (total <= shown) {
        return(shiny::tags$p(
          class = "text-muted text-center small mt-2",
          sprintf("Showing all %s voters.", format(total, big.mark = ","))
        ))
      }
      
      remaining <- total - shown
      bump <- min(200L, remaining)
      
      shiny::tags$div(
        class = "text-center mt-2",
        shiny::tags$p(
          class = "text-muted small",
          sprintf("Showing %s of %s voters.",
                  format(shown, big.mark = ","),
                  format(total, big.mark = ","))
        ),
        shiny::actionButton(session$ns("load_more"),
                            sprintf("Load %s more", format(bump, big.mark = ",")),
                            class = "btn-outline-primary")
      )
    })
  })
}