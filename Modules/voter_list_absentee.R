# ══════════════════════════════════════════════════════════════════════════
# voter_list_absentee.R
#
# "General Absentee" tab -- voters who requested a mail ballot for the
# General election (voter_mail_ballot, election = "General2026"), scoped to
# the user's role like searchable_voters(), with a precinct filter since
# admin-level users otherwise see every precinct across every scenario.
# ══════════════════════════════════════════════════════════════════════════

generalAbsenteeFiltersUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    tags$b("Precinct"),
    shiny::selectInput(ns("precinct"), NULL, choices = c("All precincts" = ""))
  )
}

generalAbsenteeTableUI <- function(id) {
  ns <- shiny::NS(id)
  DT::DTOutput(ns("table"))
}

generalAbsenteeServer <- function(id, con, searchable_voters, data_refresh,
                                   election = "General2026") {
  shiny::moduleServer(id, function(input, output, session) {

    # Base set: this user's scope, joined to General2026 mail ballot
    # requesters, before the precinct filter is applied. Also drives the
    # precinct dropdown's choices, so it only ever lists precincts that
    # actually have someone on this list -- not every precinct you canvass.
    base_matches <- shiny::reactive({
      data_refresh()

      requesters <- DBI::dbGetQuery(con,
        "SELECT voter_id, request_date, application_status, mail_ballot_status
         FROM voter_mail_ballot WHERE election = ?",
        params = list(election))

      if (nrow(requesters) == 0) return(tibble::tibble())

      v <- searchable_voters() |>
        dplyr::mutate(VoterId = as.character(VoterId)) |>
        dplyr::filter(VoterId %in% requesters$voter_id)

      if (nrow(v) == 0) return(tibble::tibble())

      v |> dplyr::left_join(requesters, by = c("VoterId" = "voter_id"))
    })

    # ── Populate precinct dropdown from whatever's actually on this list ──
    shiny::observe({
      bm <- base_matches()
      if (nrow(bm) == 0) {
        choices <- c("All precincts" = "")
      } else {
        precincts <- sort(unique(as.character(bm$Precinct)))
        choices <- c("All precincts" = "", stats::setNames(precincts, precincts))
      }
      shiny::updateSelectInput(session, "precinct", choices = choices)
    })

    output$table <- DT::renderDT({
      bm <- base_matches()

      if (nrow(bm) == 0) {
        return(DT::datatable(
          tibble::tibble(Notice = paste("No", election, "mail ballot requests on file yet.")),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      if (nzchar(input$precinct %||% "")) {
        bm <- bm |> dplyr::filter(as.character(Precinct) == input$precinct)
      }

      if (nrow(bm) == 0) {
        return(DT::datatable(
          tibble::tibble(Notice = "No matches for this precinct."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      scope_vids <- bm$VoterId

      contacted_ids <- voters_with_any_contact(con)

      material_ids <- DBI::dbGetQuery(con,
        "SELECT DISTINCT voter_id FROM contacts WHERE left_material = 'Yes'")$voter_id

      addl_notes <- dplyr::tbl(con, "additional_info") |>
        dplyr::filter(voter_id %in% !!scope_vids, !is.na(additional_text)) |>
        dplyr::select(voter_id, additional_text) |>
        dplyr::collect() |>
        dplyr::group_by(voter_id) |>
        dplyr::summarise(Notes = paste(additional_text, collapse = " | "), .groups = "drop")

      bm |>
        dplyr::left_join(addl_notes, by = c("VoterId" = "voter_id")) |>
        dplyr::mutate(
          Street          = display_address(Street),
          Contacted       = ifelse(VoterId %in% contacted_ids, "Yes", "No"),
          GivenMaterial   = ifelse(VoterId %in% material_ids, "Yes", "No"),
          Notes           = ifelse(is.na(Notes), "", Notes),
          PipelineNote    = ifelse(is.na(Additional), "", Additional),
          # Numeric sort key from StreetNumber -- handles values like "10A"
          # by sorting on the leading digits so 9 comes before 10, not after.
          StreetNumSort   = suppressWarnings(as.numeric(gsub("[^0-9]", "", StreetNumber)))
        ) |>
        dplyr::select(Name, Street, StreetName, Precinct, mail_ballot_status,
                      Contacted, GivenMaterial, Notes, PipelineNote, StreetNumSort) |>
        dplyr::arrange(StreetName, StreetNumSort) |>
        dplyr::select(-StreetNumSort) |>
        dplyr::rename(
          `Street Name`     = StreetName,
          `Mail Status`     = mail_ballot_status,
          `Given Material`  = GivenMaterial,
          `Additional Info` = Notes,
          `Pipeline Note`   = PipelineNote
        ) |>
        DT::datatable(options = list(pageLength = 50, dom = "tip"), rownames = FALSE)
    })
  })
}
