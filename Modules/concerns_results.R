# ══════════════════════════════════════════════════════════════════════════
# concernsResults.R
#
# "Results" tab: bar chart of concerns mentioned by canvassed voters,
# grouped by precinct / party / support rating / canvasser.
#
# Data scope: all voters in the user's role scope (via searchable_voters),
# latest contact per voter.
#
# Usage in app.R:
#   source("Modules/concernsResults.R")
#
#   # UI — inside your navset_tab():
#   nav_panel("Results",
#     icon = bsicons::bs_icon("bar-chart-fill"),
#     concernsResultsUI("concerns_mod")
#   )
#
#   # Server:
#   concernsResultsServer("concerns_mod",
#     con               = con,
#     searchable_voters = searchable_voters,
#     credentials       = credentials,
#     data_refresh      = data_refresh
#   )
# ══════════════════════════════════════════════════════════════════════════

kevin_palette <- c(
  "#3A4555",  # dark blue (primary)
  "#729AB9",  # light blue (secondary)
  "#6E120B",  # dark red (danger)
  "#50844C",  # NS green
  "#447DC0",  # town flag blue
  "#FBF1D8"   # cream (sidebar fg)
)

support_colors <- c(
  "1" = "#3A4555",  # strong support — primary dark blue
  "2" = "#447DC0",  # lean support — town blue
  "3" = "#729AB9",  # und/lean support — lighter blue
  "4" = "#C9A86A",  # und/lean oppose — muted warm
  "5" = "#A84E2A",  # lean oppose — orange-red
  "6" = "#6E120B"   # strong oppose — danger
)

support_labels <- c("1" = "You have my vote", "2" = "Lean for",
                    "3" = "No opinion", "4" = "Lean against",
                    "5" = "Strongly against", "6" = "Strongly against (old)")
party_colors <- c(
  "D" = "#447DC0",  # lean support — town blue
  "NA" = "#50844C",  # und/lean support — lighter blue
  "U" = "#C9A86A",  # und/lean oppose — muted warm
  "R" = "#6E120B"   # strong oppose — danger
)

# Ramp to any length, skipping the cream (too light for fills on white)
kevin_colors <- function(n) {
  base <- c("#3A4555", "#729AB9", "#6E120B", "#50844C", "#447DC0")
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

concernsResultsUI <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::card(
    bslib::card_header("Concerns Results"),
    bslib::layout_columns(
      col_widths = c(3, 9),
      
      # Left: options
      bslib::card(
        class = "bg-light",
        bslib::card_body(
          shiny::tags$strong("Group by"),
          shiny::radioButtons(
            ns("group_by"),
            label = NULL,
            choices = c("Overall"        = "overall",
                        "Precinct"       = "precinct",
                        "Party"          = "party",
                        "Support Rating" = "support",
                        "Canvasser"      = "canvasser"),
            selected = "overall"
          ),
          
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'party'", ns("group_by")),
            shiny::tags$hr(),
            shiny::checkboxGroupInput(
              ns("party_filter"),
              "Include parties:",
              choices = c("Democrat" = "D", "Unaffiliated" = "U",
                          "Republican" = "R", "Unknown" = "NA"),
              selected = c("D", "U", "R", "NA")
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'support'", ns("group_by")),
            shiny::tags$hr(),
            shiny::checkboxGroupInput(
              ns("support_filter"),
              "Include ratings:",
              choices = c(
                "1 \u2013 You have my vote" = "1",
                "2 \u2013 Lean for" = "2",
                "3 \u2013 No opinion" = "3",
                "4 \u2013 Lean against" = "4",
                "5 \u2013 Strongly against" = "5"),
              selected = c("1", "2", "3", "4", "5")
            )
          ),
          
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'canvasser'", ns("group_by")),
            shiny::tags$hr(),
            shiny::uiOutput(ns("canvasser_filter_ui"))
          ),
          
          shiny::tags$hr(),
          shinyjs::hidden(
            shiny::div(id = ns("min_n_wrapper"),
                       shiny::numericInput(ns("min_n"),
                                           "Minimum sample size per group",
                                           value = 1, min = 1, max = 100, step = 1),
                       shiny::tags$p(class = "text-muted small",
                                     "Groups with fewer voters than this are hidden.")
            )
          )
        )
      ),
      
      # Right: chart + caption
      bslib::card(
        bslib::card_body(
          shiny::uiOutput(ns("summary_caption")),
          shiny::plotOutput(ns("plot"), height = "500px"),
          shiny::tags$hr(),
          shiny::tags$details(
            shiny::tags$summary(
              style = "cursor: pointer; font-weight: 500;",
              "\u201COther\u201D responses (free text)"
            ),
            shiny::tags$div(style = "margin-top: 0.5rem;",
                            wordcloud2::wordcloud2Output(ns("other_cloud"), height = "500px"),
                            shiny::tags$hr(),
                            DT::DTOutput(ns("other_table")))
          )
        )
      )
    )
  )
}

concernsResultsServer <- function(id, con, searchable_voters,
                                  credentials, data_refresh) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # ── Dynamic canvasser checkbox list ──────────────────────────────
    output$canvasser_filter_ui <- shiny::renderUI({
      shiny::req(credentials()$user_auth)
      data_refresh()
      
      scope_vids <- as.character(searchable_voters()$VoterId)
      if (length(scope_vids) == 0) {
        return(shiny::tags$p(class = "text-muted",
                             "No voters in scope."))
      }
      
      canvassers <- dplyr::tbl(con, "contacts") |>
        dplyr::filter(voter_id %in% !!scope_vids,
                      !is.na(input_by)) |>
        dplyr::distinct(input_by) |>
        dplyr::pull(input_by)
      
      if (length(canvassers) == 0) {
        return(shiny::tags$p(class = "text-muted",
                             "No canvassers have recorded contacts yet."))
      }
      
      labels <- vapply(canvassers, user_campaigner, character(1))
      choices <- stats::setNames(canvassers,
                                 paste0(labels, " (", canvassers, ")"))
      
      shiny::checkboxGroupInput(session$ns("canvasser_filter"),
                                "Include canvassers:",
                                choices = choices,
                                selected = canvassers)
    })
    
    # ── Core data reactive ───────────────────────────────────────────
    # Long-format: one row per (voter, concern mention), with facet
    # columns attached. Scoped to searchable_voters() for role safety.
    concerns_long <- shiny::reactive({
      shiny::req(credentials()$user_auth)
      data_refresh()
      
      scope_vids <- as.character(searchable_voters()$VoterId)
      if (length(scope_vids) == 0) return(tibble::tibble())
      
      latest_per_voter <- dplyr::tbl(con, "contacts") |>
        dplyr::filter(voter_id %in% !!scope_vids) |>
        dplyr::select(voter_id, contact_timestamp, input_by,
                      concern1, concern2, concern3) |>
        dplyr::collect() |>
        dplyr::group_by(voter_id) |>
        dplyr::slice_max(contact_timestamp, n = 1,
                         with_ties = FALSE) |>
        dplyr::ungroup()
      
      if (nrow(latest_per_voter) == 0) return(tibble::tibble())
      
      long <- latest_per_voter |>
        tidyr::pivot_longer(
          cols = c(concern1, concern2, concern3),
          names_to = "slot",
          values_to = "concern"
        ) |>
        dplyr::filter(!is.na(concern), concern != "",
                      concern != "(none)") |>
        dplyr::distinct(voter_id, concern, .keep_all = TRUE)
      
      # Attach facet columns
      latest_support <- latest_field_map(con, "support_rating")
      sv <- searchable_voters() |>
        dplyr::select(VoterId, Precinct, Party) |>
        dplyr::mutate(
          VoterId = as.character(VoterId),
          SupportRating = unname(latest_support[VoterId])
        )
      
      long |>
        dplyr::left_join(sv, by = c("voter_id" = "VoterId"))
    })
    
    # ── Summary: count + pct by group ────────────────────────────────
    concerns_summary <- shiny::reactive({
      long <- concerns_long()
      if (nrow(long) == 0) return(tibble::tibble())
      
      group_by_var <- input$group_by
      min_n <- input$min_n %||% 1L
      
      # Contacted universe: voters in scope who have any contact recorded.
      # This is the denominator for all % calculations — "% of voters we
      # talked to who raised this concern", not "% of voters in the district".
      contacted_ids <- voters_with_any_contact(con)
      contacted_universe <- searchable_voters() |>
        dplyr::mutate(VoterId = as.character(VoterId)) |>
        dplyr::filter(VoterId %in% contacted_ids)
      
      
      # Pick grouping column + universe (denominator) accordingly
      if (group_by_var == "overall") {
        n_contacted <- nrow(contacted_universe)
        
        summary_df <- long |>
          dplyr::distinct(voter_id, concern) |>
          dplyr::count(concern, name = "n_mentions") |>
          dplyr::mutate(
            group = "Overall",
            n_voters = n_contacted,
            pct = n_mentions / n_voters
          )
        
      } else if (group_by_var == "precinct") {
        universe <- contacted_universe |>
          dplyr::count(Precinct, name = "n_voters") |>
          dplyr::rename(group = Precinct) #|>
          #dplyr::mutate(group = as.factor(group)) 
        summary_df <- long |>
          dplyr::distinct(voter_id, concern, Precinct) |>
          dplyr::count(Precinct, concern, name = "n_mentions") |>
          dplyr::rename(group = Precinct) |>
          dplyr::left_join(universe, by = "group") |>
          dplyr::mutate(pct = n_mentions / n_voters)
        
      } else if (group_by_var == "party") {
        keep <- input$party_filter %||% character(0)
        universe <- contacted_universe |>
          dplyr::mutate(Party = dplyr::if_else(is.na(Party), "NA", Party)) |>
          dplyr::filter(Party %in% keep) |>
          dplyr::count(Party, name = "n_voters") |>
          dplyr::rename(group = Party)
        summary_df <- long |>
          dplyr::mutate(Party = dplyr::if_else(is.na(Party), "NA", Party)) |>
          dplyr::filter(Party %in% keep) |>
          dplyr::distinct(voter_id, concern, Party) |>
          dplyr::count(Party, concern, name = "n_mentions") |>
          dplyr::rename(group = Party) |>
          dplyr::left_join(universe, by = "group") |>
          dplyr::mutate(pct = n_mentions / n_voters)
        
      } else if (group_by_var == "support") {
        keep <- input$support_filter %||% character(0)
        # Support denominator: voters in scope with a known support rating
        latest_support <- latest_field_map(con, "support_rating")
        sv <- contacted_universe |>
          dplyr::mutate(SupportRating = unname(latest_support[VoterId])) |>
          dplyr::filter(!is.na(SupportRating),
                        SupportRating %in% keep)
        universe <- sv |>
          dplyr::count(SupportRating, name = "n_voters") |>
          dplyr::rename(group = SupportRating)
        summary_df <- long |>
          dplyr::filter(!is.na(SupportRating),
                        SupportRating %in% keep) |>
          dplyr::distinct(voter_id, concern, SupportRating) |>
          dplyr::count(SupportRating, concern, name = "n_mentions") |>
          dplyr::rename(group = SupportRating) |>
          dplyr::left_join(universe, by = "group") |>
          dplyr::mutate(pct = n_mentions / n_voters)
        
      } else if (group_by_var == "canvasser") {
        keep <- input$canvasser_filter %||% character(0)
        # Canvasser denominator: voters THIS canvasser contacted
        # (not scope precinct population — see app.R comment history)
        scope_vids <- as.character(searchable_voters()$VoterId)
        canvasser_contacts <- dplyr::tbl(con, "contacts") |>
          dplyr::filter(voter_id %in% !!scope_vids,
                        !is.na(input_by),
                        input_by %in% !!keep) |>
          dplyr::select(voter_id, input_by) |>
          dplyr::distinct() |>
          dplyr::collect()
        universe <- canvasser_contacts |>
          dplyr::count(input_by, name = "n_voters") |>
          dplyr::rename(group = input_by)
        summary_df <- long |>
          dplyr::filter(input_by %in% keep) |>
          dplyr::distinct(voter_id, concern, input_by) |>
          dplyr::count(input_by, concern, name = "n_mentions") |>
          dplyr::rename(group = input_by) |>
          dplyr::left_join(universe, by = "group") |>
          dplyr::mutate(pct = n_mentions / n_voters)
        
      }
      
      summary_df |>
        dplyr::filter(n_voters >= min_n)
      
      
      
    })
    
    # ── Plot ─────────────────────────────────────────────────────────
    output$plot <- shiny::renderPlot({
      df <- concerns_summary()
      if (nrow(df) == 0) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0, y = 0,
                              label = "No concerns recorded yet.") +
            ggplot2::theme_void()
        )
      }
      
      # Map support ratings + canvasser usernames to readable labels
      if (input$group_by == "support") {
        df$group <- factor(df$group,
                           levels = c("1","2","3","4","5","6"),
                           labels = support_labels[c("1","2","3","4","5","6")])
      } else if (input$group_by == "canvasser") {
        df$group <- vapply(df$group, user_campaigner, character(1))
      } else if (input$group_by == "precinct") {
        df$group <- factor(df$group)
      } else if (input$group_by == "party") {
        party_labels <- c("D"  = "Democrat",
                          "U"  = "Unaffiliated",
                          "R"  = "Republican",
                          "NA" = "Unknown")
        df$group <- factor(df$group,
                           levels = c("D", "U", "R", "NA"),
                           labels = party_labels[c("D", "U", "R", "NA")])
      }
    
      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x = pct,
                                        y = stats::reorder(concern, pct))) +
        ggplot2::scale_x_continuous(labels = scales::percent) + 
        ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = "#FFFFFF", color = NA),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),  # horizontal bars don't need y gridlines
          axis.text        = ggplot2::element_text(color = "#3A4555"),
          legend.position  = "bottom",
          legend.title     = ggplot2::element_blank()
        )
      
      if (input$group_by == "overall") {
        p + ggplot2::geom_col(fill = "#3A4555")
      } else if (input$group_by == "support") {
        scale_values <- setNames(unname(support_colors),
                                 support_labels[names(support_colors)])
        p + 
          ggplot2::geom_col(
            ggplot2::aes(fill = group),
            position = ggplot2::position_dodge(preserve = "single")
          ) +
          ggplot2::scale_fill_manual(values = scale_values)
      } else if (input$group_by == "party") {
        scale_values <- setNames(unname(party_colors),
                                 party_labels[names(party_colors)])
        p + 
          ggplot2::geom_col(
            ggplot2::aes(fill = group),
            position = ggplot2::position_dodge(preserve = "single")
          ) +
          ggplot2::scale_fill_manual(values = scale_values) 
        } else {
        
        n_groups <- length(unique(df$group))
        p + ggplot2::geom_col(
          ggplot2::aes(fill = group),
          position = ggplot2::position_dodge(preserve = "single")) +
          ggplot2::scale_fill_manual(values = kevin_colors(n_groups))
      }
    })
    
    # Wordcloud
    output$other_cloud <- wordcloud2::renderWordcloud2({
      shiny::req(credentials()$user_auth)
      data_refresh()
      
      scope_vids <- as.character(searchable_voters()$VoterId)
      if (length(scope_vids) == 0) return(NULL)
      
      # Same "Other" extraction as other_table — could refactor into a shared reactive
      latest_per_voter <- dplyr::tbl(con, "contacts") |>
        dplyr::filter(voter_id %in% !!scope_vids) |>
        dplyr::select(voter_id, contact_timestamp,
                      concern1, concern1_other,
                      concern2, concern2_other,
                      concern3, concern3_other) |>
        dplyr::collect() |>
        dplyr::group_by(voter_id) |>
        dplyr::slice_max(contact_timestamp, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
      
      other_texts <- purrr::map_chr(1:3, function(i) {
        c_col <- paste0("concern", i)
        o_col <- paste0("concern", i, "_other")
        rows <- latest_per_voter |>
          dplyr::filter(.data[[c_col]] == "Other",
                        !is.na(.data[[o_col]]),
                        nzchar(.data[[o_col]]))
        paste(rows[[o_col]], collapse = " ")
      }) |> paste(collapse = " ")
      
      freq <- tokenize_other_text(other_texts)
      if (nrow(freq) == 0) return(NULL)
      
      wordcloud2::wordcloud2(freq, size = 0.3,
                             color = rep(c("#3A4555", "#729AB9", "#6E120B"),
                                         length.out = nrow(freq)))
    })
    
    
    
    # ── Caption ─────────────────────────────────────────────────────
    output$summary_caption <- shiny::renderUI({
      data_refresh()
      n_scope <- nrow(searchable_voters())
      long <- concerns_long()
      n_with_concerns <- if (nrow(long) == 0) 0L else dplyr::n_distinct(long$voter_id)
      shiny::tags$p(
        class = "text-muted small",
        sprintf("Latest recorded concerns from %s voters (of %s in scope).",
                format(n_with_concerns, big.mark = ","),
                format(n_scope, big.mark = ","))
      )
    })
    
    # ── "Other" free-text table ─────────────────────────────────────
    output$other_table <- DT::renderDT({
      shiny::req(credentials()$user_auth)
      data_refresh()
      
      scope_vids <- as.character(searchable_voters()$VoterId)
      if (length(scope_vids) == 0) return(DT::datatable(tibble::tibble()))
      
      latest_per_voter <- dplyr::tbl(con, "contacts") |>
        dplyr::filter(voter_id %in% !!scope_vids) |>
        dplyr::select(voter_id, contact_timestamp, input_by,
                      concern1, concern1_other,
                      concern2, concern2_other,
                      concern3, concern3_other) |>
        dplyr::collect() |>
        dplyr::group_by(voter_id) |>
        dplyr::slice_max(contact_timestamp, n = 1,
                         with_ties = FALSE) |>
        dplyr::ungroup()
      
      # Collect only rows where a concern was "Other" with non-empty text
      others <- purrr::map_dfr(
        1:3,
        function(i) {
          c_col <- paste0("concern", i)
          o_col <- paste0("concern", i, "_other")
          latest_per_voter |>
            dplyr::filter(.data[[c_col]] == "Other",
                          !is.na(.data[[o_col]]),
                          nzchar(.data[[o_col]])) |>
            dplyr::transmute(
              Canvasser = input_by,
              Text = .data[[o_col]],
              Date = substr(contact_timestamp, 1, 10)
            )
        }
      )
      
      DT::datatable(others,
                    options = list(pageLength = 10, dom = "tip"),
                    rownames = FALSE)
    })
  })
}

latest_contacts_in_scope <- shiny::reactive({
  data_refresh()
  scope_vids <- as.character(searchable_voters()$VoterId)
  if (length(scope_vids) == 0) return(tibble::tibble())
  
  dplyr::tbl(con, "contacts") |>
    dplyr::filter(voter_id %in% !!scope_vids) |>
    dplyr::collect() |>
    dplyr::group_by(voter_id) |>
    dplyr::slice_max(contact_timestamp, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
})



