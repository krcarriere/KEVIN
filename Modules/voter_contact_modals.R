# ══════════════════════════════════════════════════════════════════════════
# voter_contact_modals.R  (DB-native)
#
# Modal dialog flows for the voter info panel:
#   • "Different Person" → confirms known voters, QR/email registration,
#     Record New Contact form → save_new_contact
#       insert_new_contact() atomically writes new_contacts + voters stub
#       + initial contacts row. flag_may_have_moved() marks same-door
#       voters.
#   • "Add Info" → lightweight support/notes modal → save_addl_info
#       insert_additional_info() logs + merges who_knows;
#       append_additional() extends voters.additional;
#       insert_contact() if a support rating was given.
#
# The trigger buttons (actionLink "diff_person" and "add_info") live in
# output$voter_info_panel in KEVIN.R.
#
# Usage in KEVIN.R:
#   source("Modules/voterContactModals.R")
#   registerVoterContactModals(
#     input, output, session,
#     current_voter   = current_voter,
#     route_data      = route_data,
#     data_refresh    = data_refresh,
#     credentials     = credentials,
#     con             = con,
#     who_initials    = who_initials,
#     concern_choices = concern_choices
#   )
# ══════════════════════════════════════════════════════════════════════════

registerVoterContactModals <- function(input, output, session,
                                       current_voter,
                                       route_data,
                                       voter_data,
                                       data_refresh,
                                       credentials,
                                       con,
                                       who_initials,
                                       concern_choices,
                                       user_scenarios,
                                       user_role) {
  
  # ── "Different Person" → list other voters at this stop ────────────────
  observeEvent(input$diff_person, {
    v <- current_voter()
    if (is.null(v)) return()
    
    rd <- route_data()
    
    if (nrow(rd) > 0 && !"DisplayAddress" %in% names(rd)) {
      rd$DisplayAddress <- rd$Street
    }
    
    others <- rd |>
      dplyr::filter(Precinct  == v$Precinct,
                    Street == v$Street,
                    VoterId   != v$VoterId)
    
    if (nrow(others) > 0) {
      name_list <- tags$ul(
        lapply(seq_len(nrow(others)), function(i) {
          tags$li(paste0(others$FirstName[i], " ", others$LastName[i],
                         " (", others$Party[i], ", Age ", others$Age[i], ")"))
        })
      )
    } else {
      name_list <- tags$p(tags$em("No other registered voters at this address."))
    }
    
    showModal(modalDialog(
      title = "Different person answered?",
      tags$p("Was it any of these people at this address?"),
      name_list,
      tags$p("If yes, close this and scroll to them."),
      footer = tagList(
        actionButton("diff_person_yes", "Yes, it's one of them", class = "btn-secondary"),
        actionButton("diff_person_no",  "No, someone new",        class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$diff_person_yes, { removeModal() })
  
  # "No" → registration QR + mailto modal
  observeEvent(input$diff_person_no, {
    removeModal()
    
    showModal(modalDialog(
      title = "Let's Get You Registered!",
      tags$div(style = "text-align: center;",
               tags$p("Scan this QR code or click Email Link and we'll send you a link."),
               
               layout_columns(
                 col_widths = c(6, 6),
                 tags$div(style = "text-align: center;",
                          tags$strong("vote.sos.ri.gov"),
                          plotOutput("qr_code_plot", height = "200px", width = "200px")
                 ),
                 tags$div(style = "text-align: center; padding-top: 40px;",
                          tags$strong("Send link to Email"),
                          tags$br(), tags$br(),
                          tags$a(
                            class = "btn btn-danger",
                            href = paste0(
                              "mailto:?bcc=northsmithfielddtc@gmail.com",
                              "&subject=Register%20to%20Vote%20Link",
                              "&body=Hi%2C%20it%20was%20nice%20meeting%20you.%0D%0A%0D%0A",
                              "If%20you%E2%80%99re%20not%20registered%20to%20vote%2C%20you%20can%20do%20so%20here%3A%0D%0A",
                              "https%3A%2F%2Fvote.sos.ri.gov%2FHome%2FRegistertoVote%0D%0A%0D%0A",
                              "If%20you%E2%80%99ve%20moved%20or%20want%20to%20check%20your%20registration%2C%20go%20here%3A%0D%0A",
                              "https%3A%2F%2Fvote.sos.ri.gov%2FHome%2FUpdateVoterRecord%0D%0A%0D%0A",
                              "Deadlines%3A%0D%0A%0D%0A",
                              "Primary%3A%20September%205%0D%0A",
                              "General%20election%3A%20October%204%2C%202026"
                            ),
                            target = "_blank",
                            bsicons::bs_icon("envelope-fill"), " Email Link"
                          )
                 )
               )
      ),
      footer = tags$div(style = "text-align: center; width: 100%;",
                        actionButton("diff_person_continue", "Continue", class = "btn-primary")
      ),
      size      = "m",
      easyClose = TRUE
    ))
  })
  
  output$qr_code_plot <- renderPlot({
    qr <- qrcode::qr_code("https://vote.sos.ri.gov/Home/RegistertoVote")
    plot(qr)
  }, bg = "transparent")
  
  # ── Record New Contact form ────────────────────────────────────────────
  observeEvent(input$diff_person_continue, {
    removeModal()
    showModal(modalDialog(
      title = "Record new contact",
      # Check which existing voters at this door may have also moved out.
      # Default everyone to unchecked — canvasser ticks only the ones
      # they've confirmed aren't living there anymore.
      uiOutput("same_door_checkboxes"),
      
      textInput("new_first", "First Name"),
      textInput("new_last",  "Last Name"),
      radioButtons("new_left_material", "Left material?",
                   choices = c("Yes", "No"),
                   selected = "Yes", inline = TRUE),
      selectInput("new_support", "Support Rating",
                  choices = c(
                    "Select..." = "",
                    "1 \u2013 You have my vote" = "1",
                    "2 \u2013 Lean for" = "2",
                    "3 \u2013 No opinion" = "3",
                    "4 \u2013 Lean against" = "4",
                    "5 \u2013 Strongly against" = "5"),
                  selected = ""),
      
      selectInput("new_concern1", "Concern #1",
                  choices = concern_choices, selected = "(none)"),
      uiOutput("new_concern1_other_ui"),
      selectInput("new_concern2", "Concern #2",
                  choices = concern_choices, selected = "(none)"),
      uiOutput("new_concern2_other_ui"),
      selectInput("new_concern3", "Concern #3",
                  choices = concern_choices, selected = "(none)"),
      uiOutput("new_concern3_other_ui"),
      
      textInput("new_notes", "Notes"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_contact", "Save", class = "btn-primary")
      ),
      size = "m", easyClose = FALSE
    ))
  })
  
  # Per-voter "may have moved" checkboxes for the Record New Contact modal.
  # The list is computed from route_data at modal-open time; we save it to
  # a reactiveVal so the save handler can read the same snapshot.
  output$same_door_checkboxes <- renderUI({
    v <- current_voter()
    if (is.null(v)) return(NULL)
    rd <- route_data()
    
    if (nrow(rd) > 0 && !"DisplayAddress" %in% names(rd)) {
      rd$DisplayAddress <- rd$Street
    }
    
    
    same_door <- get_same_door_voters(v, route_data()) # db_helpers()
    
    if (nrow(same_door) == 0) {
      return(tags$p(tags$em("No other registered voters at this address.")))
    }
    # Build a named vector for checkboxGroupInput: "Jane Smith (D)" = "12345"
    choice_labels <- paste0(same_door$FirstName, " ", same_door$LastName,
                            " (", same_door$Party, ")")
    choices <- stats::setNames(as.character(same_door$VoterId), choice_labels)
    tagList(
      tags$label(class = "form-label",
                 "Did any of these registered voters move out?"),
      tags$p(class = "text-muted small",
             "Leave unchecked if they still live here (e.g. roommate, family member still present)."),
      checkboxGroupInput("moved_voter_ids", label = NULL,
                         choices = choices,
                         selected = character(0))
    )
  })
  
  
  # ── Save New Contact (DB) ───────────────────────────────────────────────
  observeEvent(input$save_new_contact, {
    
    
    v <- current_voter()
    if (is.null(v)) return()
    req(v)
    rd <- route_data()
    
    if (nrow(rd) > 0 && !"DisplayAddress" %in% names(rd)) {
      rd$DisplayAddress <- rd$Street
    }
    
    # Sanitize user-typed text (formula-injection / control-char defense)
    first_name <- if (nzchar(trimws(input$new_first %||% ""))) sanitize_user_input(trimws(input$new_first)) else NA_character_
    last_name  <- if (nzchar(trimws(input$new_last  %||% ""))) sanitize_user_input(trimws(input$new_last))  else NA_character_
    notes_val  <- if (nzchar(trimws(input$new_notes %||% ""))) sanitize_user_input(trimws(input$new_notes)) else NA_character_
    
    resolve_other <- function(concern_in, other_in) {
      if (isTRUE(concern_in == "Other") &&
          !is.null(other_in) && nzchar(trimws(other_in))) {
        sanitize_user_input(other_in)
      } else NA_character_
    }
    c1_other <- resolve_other(input$new_concern1, input$new_concern1_other)
    c2_other <- resolve_other(input$new_concern2, input$new_concern2_other)
    c3_other <- resolve_other(input$new_concern3, input$new_concern3_other)
    

    new_left_material_val <- validate_choice(
      input$new_left_material,
      validators$left_material_choice
    )
    new_support_rating_val <- validate_choice(
      input$new_support,
      validators$support_rating_choice
    )
    new_concern1_val <- validate_choice(
      input$new_concern1,
      validators$concern1_choice
    )
    new_concern2_val <- validate_choice(
      input$new_concern2,
      validators$concern2_choice
    )
    new_concern3_val <- validate_choice(
      input$new_concern3,
      validators$concern3_choice
    )
    
    scen_val <- if (!is.null(input$scenario) && 
                    input$scenario %in% user_scenarios()) {
      input$scenario
    } else {
      NA_character_
    }
    
    interaction_fields <- list(
      campaigner       = credentials()$info$user,
      left_material    = new_left_material_val,
      were_they_there  = "Yes",   # met in person by definition
      had_conversation = "Yes", # By definition
      support_rating   = new_support_rating_val,
      concern1         = new_concern1_val, 
      concern1_other   = c1_other,
      concern2         = new_concern2_val,
      concern2_other   = c2_other,
      concern3         = new_concern3_val,
      concern3_other   = c3_other,
      notes            = notes_val
    )
    
    new_vid <- insert_new_contact(
      con,
      recorded_by = credentials()$info$user,
      first_name  = first_name,
      last_name   = last_name,
      address     = v$Street,
      precinct    = v$Precinct,
      route       = v$Route,
      stop_order  = v$StopOrder,
      scenario    = scen_val,
      interaction_fields = interaction_fields
    )
    
    # Flag only the voters the canvasser explicitly marked as moved out.
    # Default is no one — adding a new contact does not automatically
    # imply existing residents moved (think: new roommate, grandma
    # moving in with the family, new baby).
    same_door <- get_same_door_voters(v, rd)
    valid_ids <- as.character(same_door$VoterId)
    moved_ids <- input$moved_voter_ids %||% character(0)
    moved_ids <- intersect(as.character(moved_ids), valid_ids)
    
    if (length(moved_ids) > 0) {
      flag_may_have_moved(con, moved_ids)
      
    # Also update in-memory voters_raw so 📦 emoji shows without restart
      vr <- get("voters_raw", envir = globalenv())
      moved_mask <- vr$VoterId %in% moved_ids
      vr$MayHaveMoved[moved_mask] <- 1L
      assign("voters_raw", vr, envir = globalenv())
    }
    
    # Append the new contact to in-memory voters_raw + routes_raw so the
    # route listing, street search, and name search see them immediately.
    full_name <- trimws(paste(first_name, last_name))
    
    # Parse street for upper/lower search fields
    addr_title <- stringr::str_to_title(v$Street %||% "")
    addr_title <- stringr::str_replace(addr_title, "Ri,", "RI,")
    street_sfx <- stringr::str_extract(addr_title, "\\S+(?=,)")
    street_nm  <- stringr::str_match(addr_title, "^\\d+\\s([^,]+)")[, 2]
    street_nm  <- stringr::str_remove(street_nm, street_sfx %||% "")
    street_nm  <- stringr::str_to_title(street_nm)
    
    new_voter_row <- data.frame(
      VoterId           = as.character(new_vid),
      FirstName         = first_name %||% NA_character_,
      LastName          = last_name  %||% NA_character_,
      FullName          = full_name,
      Name              = full_name,
      FullAddress       = addr_title,
      Street            = addr_title,
      Precinct          = v$Precinct,
      Party             = NA_character_,
      PartyCode         = NA_character_,
      VoterScore        = NA_real_,
      TurnoutScore      = NA_real_,
      Age               = NA_integer_,
      VoterStatus       = NA_character_,
      WhoKnows          = NA_character_,
      Additional        = NA_character_,
      DonateText        = NA_character_,
      nsse_2026_voter   = NA_character_,
      TimesSpoken       = 0L,
      MayHaveMoved      = 0L,
      StreetNum = suppressWarnings(as.integer(stringr::str_match(addr_title, "^\\s*(\\d+)")[,2])),
      StreetName        = street_nm,
      StreetSuffix      = street_sfx,
      StreetNameUpper   = toupper(trimws(street_nm %||% "")),
      StreetSuffixUpper = toupper(trimws(street_sfx %||% "")),
      FirstNameUpper    = toupper(trimws(first_name %||% "")),
      LastNameUpper     = toupper(trimws(last_name %||% "")),
      RowID             = max(get("voters_raw", envir = globalenv())$RowID, na.rm = TRUE) + 1L,
      stringsAsFactors  = FALSE
    )
    assign("voters_raw",
           dplyr::bind_rows(get("voters_raw", envir = globalenv()), new_voter_row),
           envir = globalenv())
    
    
    
    # Route row so route searches include them
    new_route_row <- data.frame(
      VoterId     = as.character(new_vid),
      Scenario    = scen_val,
      Precinct    = v$Precinct,
      Route       = v$Route,
      StopOrder   = v$StopOrder,
      RouteName   = v$RouteName %||% NA_character_,
      FullAddress = addr_title,
      stringsAsFactors = FALSE
    )
    assign("routes_raw",
           dplyr::bind_rows(get("routes_raw", envir = globalenv()), new_route_row),
           envir = globalenv())
    
    #save_new_contact
    # Reload the current route so the new contact appears without needing
    # to re-click Search. voter_data() was just recomputed thanks to the
    # data_refresh bump above.
    if (!is.null(v$Precinct) && !is.null(v$Route)) {
      refreshed <- voter_data() |>
        dplyr::filter(Precinct == v$Precinct, Route == v$Route) |>
        dplyr::arrange(StopOrder)
      route_data(refreshed)
      
    }
    
    data_refresh(data_refresh() + 1)
    
    removeModal()
    showNotification("New contact saved!", type = "message")
  })
  
  # ════════════════════════════════════════════════════════════════════════
  # #7 — Add a contact at a NEW (unregistered) address on the current route.
  # Admin-only. Inherits precinct / route / stop_order / street from the
  # current stop; canvasser types only the house number. Runs a whole-base
  # duplicate check before inserting so we don't create a phantom record for
  # someone who's actually registered but outside the walked scenario.
  # ════════════════════════════════════════════════════════════════════════
  
  pending_new_addr <- reactiveVal(NULL)
  
  # Shared in-memory append (mirrors the inline block in save_new_contact;
  # this one also sets StreetNum so the new door is itself dupe-checkable).
  append_inmemory_new_contact <- function(new_vid, first_name, last_name,
                                          full_addr, precinct, route,
                                          stop_order, route_name, scenario) {
    full_name  <- trimws(paste(first_name, last_name))
    addr_title <- stringr::str_to_title(full_addr %||% "")
    addr_title <- stringr::str_replace(addr_title, "Ri,", "RI,")
    street_sfx <- stringr::str_extract(addr_title, "\\S+(?=,)")
    street_nm  <- stringr::str_match(addr_title, "^\\d+\\s([^,]+)")[, 2]
    street_nm  <- stringr::str_remove(street_nm, street_sfx %||% "")
    street_nm  <- stringr::str_to_title(street_nm)
    
    new_voter_row <- data.frame(
      VoterId           = as.character(new_vid),
      FirstName         = first_name %||% NA_character_,
      LastName          = last_name  %||% NA_character_,
      FullName          = full_name,
      Name              = full_name,
      FullAddress       = addr_title,
      Street            = addr_title,
      Precinct          = precinct,
      Party             = NA_character_,
      PartyCode         = NA_character_,
      VoterScore        = NA_real_,
      TurnoutScore      = NA_real_,
      Age               = NA_integer_,
      VoterStatus       = NA_character_,
      WhoKnows          = NA_character_,
      Additional        = NA_character_,
      DonateText        = NA_character_,
      nsse_2026_voter   = NA_character_,
      TimesSpoken       = 0L,
      MayHaveMoved      = 0L,
      StreetName        = street_nm,
      StreetSuffix      = street_sfx,
      StreetNameUpper   = toupper(trimws(street_nm  %||% "")),
      StreetSuffixUpper = toupper(trimws(street_sfx %||% "")),
      StreetNum         = suppressWarnings(as.integer(
        stringr::str_match(addr_title, "^\\s*(\\d+)")[, 2])),
      FirstNameUpper    = toupper(trimws(first_name %||% "")),
      LastNameUpper     = toupper(trimws(last_name  %||% "")),
      RowID             = max(get("voters_raw", envir = globalenv())$RowID, na.rm = TRUE) + 1L,
      stringsAsFactors  = FALSE
    )
    assign("voters_raw",
           dplyr::bind_rows(get("voters_raw", envir = globalenv()), new_voter_row),
           envir = globalenv())
    
    new_route_row <- data.frame(
      VoterId     = as.character(new_vid),
      Scenario    = scenario,
      Precinct    = precinct,
      Route       = route,
      StopOrder   = stop_order,
      RouteName   = route_name %||% NA_character_,
      FullAddress = addr_title,
      stringsAsFactors = FALSE
    )
    assign("routes_raw",
           dplyr::bind_rows(get("routes_raw", envir = globalenv()), new_route_row),
           envir = globalenv())
    invisible(TRUE)
  }
  
  # The trimmed contact form (no same-door checkboxes — irrelevant at a new
  # address). Save id is "save_new_address".
  build_addr_contact_modal <- function(addr) {
    modalDialog(
      title = "Record contact at new address",
      tags$p(tags$strong("Address: "), addr),
      textInput("na_first", "First Name"),
      textInput("na_last",  "Last Name"),
      radioButtons("na_left_material", "Left material?",
                   choices = c("Yes", "No"), selected = "Yes", inline = TRUE),
      selectInput("na_support", "Support Rating",
                  choices = c(
                    "Select..." = "",
                    "1 \u2013 You have my vote" = "1",
                    "2 \u2013 Lean for" = "2",
                    "3 \u2013 No opinion" = "3",
                    "4 \u2013 Lean against" = "4",
                    "5 \u2013 Strongly against" = "5"),
                  selected = ""),
      selectInput("na_concern1", "Concern #1", choices = concern_choices, selected = "(none)"),
      uiOutput("na_concern1_other_ui"),
      selectInput("na_concern2", "Concern #2", choices = concern_choices, selected = "(none)"),
      uiOutput("na_concern2_other_ui"),
      selectInput("na_concern3", "Concern #3", choices = concern_choices, selected = "(none)"),
      uiOutput("na_concern3_other_ui"),
      textInput("na_notes", "Notes"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_address", "Save", class = "btn-primary")
      ),
      size = "m", easyClose = FALSE
    )
  }
  
  output$na_concern1_other_ui <- renderUI({
    if (isTRUE(input$na_concern1 == "Other")) textInput("na_concern1_other", NULL, placeholder = "Specify...")
  })
  output$na_concern2_other_ui <- renderUI({
    if (isTRUE(input$na_concern2 == "Other")) textInput("na_concern2_other", NULL, placeholder = "Specify...")
  })
  output$na_concern3_other_ui <- renderUI({
    if (isTRUE(input$na_concern3 == "Other")) textInput("na_concern3_other", NULL, placeholder = "Specify...")
  })
  
  # Step 1: ask for the house number.
  observeEvent(input$new_address, {
    if (!identical(user_role(), "admin")) return()
    v <- current_voter(); if (is.null(v)) return()
    showModal(modalDialog(
      title = "Add a new address on this route",
      tags$p(tags$strong("Street: "), sub("^\\s*[0-9]+\\s*", "", v$Street %||% "")),
      numericInput("new_addr_num", "House number?", value = NA, min = 1),
      tags$p(class = "text-muted small",
             "Inherits this stop's street, precinct, and route position."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("new_addr_continue", "Continue", class = "btn-primary")
      ),
      size = "s", easyClose = FALSE
    ))
  })
  
  # Step 2: whole-base duplicate check.
  observeEvent(input$new_addr_continue, {
    if (!identical(user_role(), "admin")) return()
    v <- current_voter(); if (is.null(v)) return()
    num <- suppressWarnings(as.integer(input$new_addr_num))
    if (is.na(num) || num < 1) {
      showNotification("Enter a valid house number.", type = "warning"); return()
    }
    # Build new address by swapping the leading number on the current stop.
    new_addr <- sub("^\\s*[0-9]+", as.character(num), v$Street %||% "")
    pending_new_addr(new_addr)
    
    # Match against the ENTIRE roll (all parties / scenarios). Use this
    # voter's own street fields so comparison is consistent with the base.
    vr  <- get("voters_raw", envir = globalenv())
    cur <- vr[vr$VoterId == as.character(v$VoterId), , drop = FALSE]
    nm_u  <- if (nrow(cur)) cur$StreetNameUpper[1]   else NA_character_
    sfx_u <- if (nrow(cur)) cur$StreetSuffixUpper[1] else NA_character_
    hit <- vr[!is.na(vr$StreetNum)         & vr$StreetNum == num &
                !is.na(vr$StreetNameUpper)   & vr$StreetNameUpper   == nm_u &
                !is.na(vr$StreetSuffixUpper) & vr$StreetSuffixUpper == sfx_u,
              , drop = FALSE]
    
    if (nrow(hit) > 0) {
      who <- paste0(hit$FirstName, " ", hit$LastName,
                    " (", ifelse(is.na(hit$Party), "?", hit$Party), ")")
      removeModal()
      showModal(modalDialog(
        title = "That address is already on the roll",
        tags$p(new_addr),
        tags$p("Registered here (may be outside the scenario you're walking):"),
        tags$ul(lapply(who, tags$li)),
        tags$p("Record one of them instead, or add a brand-new person anyway."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("new_addr_force", "Add new person anyway", class = "btn-warning")
        ),
        easyClose = TRUE
      ))
    } else {
      removeModal()
      showModal(build_addr_contact_modal(new_addr))
    }
  })
  
  # Step 2b: override — add anyway despite a roll match.
  observeEvent(input$new_addr_force, {
    if (!identical(user_role(), "admin")) return()
    removeModal()
    showModal(build_addr_contact_modal(pending_new_addr()))
  })
  
  # Step 3: save.
  observeEvent(input$save_new_address, {
    if (!identical(user_role(), "admin")) return()
    v <- current_voter(); if (is.null(v)) return()
    addr <- pending_new_addr()
    if (is.null(addr) || !nzchar(addr)) {
      showNotification("No address set.", type = "error"); return()
    }
    
    first_name <- if (nzchar(trimws(input$na_first %||% ""))) sanitize_user_input(trimws(input$na_first)) else NA_character_
    last_name  <- if (nzchar(trimws(input$na_last  %||% ""))) sanitize_user_input(trimws(input$na_last))  else NA_character_
    notes_val  <- if (nzchar(trimws(input$na_notes %||% ""))) sanitize_user_input(trimws(input$na_notes)) else NA_character_
    
    resolve_other <- function(concern_in, other_in) {
      if (isTRUE(concern_in == "Other") && !is.null(other_in) && nzchar(trimws(other_in)))
        sanitize_user_input(other_in) else NA_character_
    }
    c1_other <- resolve_other(input$na_concern1, input$na_concern1_other)
    c2_other <- resolve_other(input$na_concern2, input$na_concern2_other)
    c3_other <- resolve_other(input$na_concern3, input$na_concern3_other)
    
    left_val <- validate_choice(input$na_left_material, validators$left_material_choice)
    sup_val  <- validate_choice(input$na_support,       validators$support_rating_choice)
    c1_val   <- validate_choice(input$na_concern1,      validators$concern1_choice)
    c2_val   <- validate_choice(input$na_concern2,      validators$concern2_choice)
    c3_val   <- validate_choice(input$na_concern3,      validators$concern3_choice)
    
    scen_val <- if (!is.null(input$scenario) && input$scenario %in% user_scenarios()) input$scenario else NA_character_
    
    interaction_fields <- list(
      campaigner       = credentials()$info$user,
      left_material    = left_val,
      were_they_there  = "Yes",
      had_conversation = "Yes",
      support_rating   = sup_val,
      concern1 = c1_val, concern1_other = c1_other,
      concern2 = c2_val, concern2_other = c2_other,
      concern3 = c3_val, concern3_other = c3_other,
      notes    = notes_val
    )
    
    new_vid <- insert_new_contact(
      con, recorded_by = credentials()$info$user,
      first_name = first_name, last_name = last_name,
      address    = addr,
      precinct   = v$Precinct, route = v$Route, stop_order = v$StopOrder,
      scenario   = scen_val, interaction_fields = interaction_fields
    )
    
    # Persist a routes-table row so the address survives a reload and is
    # picked up by voter_data()'s VoterId+Precinct join. (voters stub was
    # written first inside insert_new_contact, so this row's voter_id resolves.)
    DBI::dbExecute(con,
                   "INSERT INTO routes
         (voter_id, scenario, precinct, full_address, route, stop_order, route_name)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
                   params = list(as.character(new_vid), scen_val,
                                 as.integer(v$Precinct), addr,
                                 as.integer(v$Route), as.integer(v$StopOrder),
                                 v$RouteName %||% NA_character_))
    
    append_inmemory_new_contact(new_vid, first_name, last_name, addr,
                                v$Precinct, v$Route, v$StopOrder,
                                v$RouteName %||% NA_character_, scen_val)
    
    # Bump FIRST so voter_data() recomputes when we read it just below.
    data_refresh(data_refresh() + 1)
    
    if (!is.null(v$Precinct) && !is.null(v$Route)) {
      refreshed <- voter_data() |>
        dplyr::filter(Precinct == v$Precinct, Route == v$Route) |>
        dplyr::arrange(StopOrder)
      route_data(refreshed)
    }
    
    pending_new_addr(NULL)
    removeModal()
    showNotification("New address contact saved!", type = "message")
  })
  
  # "Other" text fields for new-contact concerns
  output$new_concern1_other_ui <- renderUI({
    if (isTRUE(input$new_concern1 == "Other"))
      textInput("new_concern1_other", NULL, placeholder = "Specify...")
  })
  
  output$new_concern2_other_ui <- renderUI({
    if (isTRUE(input$new_concern2 == "Other"))
      textInput("new_concern2_other", NULL, placeholder = "Specify...")
  })
  output$new_concern3_other_ui <- renderUI({
    if (isTRUE(input$new_concern3 == "Other"))
      textInput("new_concern3_other", NULL, placeholder = "Specify...")
  })
  
  
  # ── "Add Info" modal ────────────────────────────────────────────────────
  observeEvent(input$add_info, {
    v <- current_voter()
    if (is.null(v)) return()
    
    showModal(modalDialog(
      title = paste0("Add Info: ", v$FirstName, " ", v$LastName),
      radioButtons("addl_know_them", "Do you know this person?",
                   choices = c("Yes" = "Y", "No" = "N"),
                   selected = "N", inline = TRUE),
      selectInput("addl_support", "Support Rating",
                  choices = c(
                    "Select..." = "",
                    "1 \u2013 You have my vote" = "1",
                    "2 \u2013 Lean for" = "2",
                    "3 \u2013 No opinion" = "3",
                    "4 \u2013 Lean against" = "4",
                    "5 \u2013 Strongly against" = "5"),
                  selected = ""),
      textInput("addl_text", "Additional notes",
                placeholder = "e.g. Coaches little league with Paul"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_addl_info", "Save", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  # ── Standalone "Reg to Vote" button — opens the QR/email modal
  # without the new-contact entry flow. Lets canvassers register 
  # walk-ups fast when they don't have time to record a full contact.
  observeEvent(input$reg_to_vote, {
    showModal(modalDialog(
      title = "Let's Get You Registered!",
      tags$div(style = "text-align: center;",
               tags$p("Scan this QR code or click Email Link and we'll send you a link."),
               
               layout_columns(
                 col_widths = c(6, 6),
                 tags$div(style = "text-align: center;",
                          tags$strong("vote.sos.ri.gov"),
                          plotOutput("qr_code_plot_standalone", height = "200px", width = "200px")
                 ),
                 tags$div(style = "text-align: center; padding-top: 40px;",
                          tags$strong("Send link to Email"),
                          tags$br(), tags$br(),
                          tags$a(
                            class = "btn btn-danger",
                            href = paste0(
                              "mailto:?bcc=northsmithfielddtc@gmail.com",
                              "&subject=Register%20to%20Vote%20Link",
                              "&body=Hi%2C%20it%20was%20nice%20meeting%20you.%0D%0A%0D%0A",
                              "If%20you%E2%80%99re%20not%20registered%20to%20vote%2C%20you%20can%20do%20so%20here%3A%0D%0A",
                              "https%3A%2F%2Fvote.sos.ri.gov%2FHome%2FRegistertoVote%0D%0A%0D%0A",
                              "If%20you%E2%80%99ve%20moved%20or%20want%20to%20check%20your%20registration%2C%20go%20here%3A%0D%0A",
                              "https%3A%2F%2Fvote.sos.ri.gov%2FHome%2FUpdateVoterRecord%0D%0A%0D%0A",
                              "Deadlines%3A%0D%0A%0D%0A",
                              "Primary%3A%20September%205%0D%0A",
                              "General%20election%3A%20October%204%2C%202026"
                            ),
                            target = "_blank",
                            bsicons::bs_icon("envelope-fill"), " Email Link"
                          )
                 )
               )
      ),
      footer = modalButton("Close"),
      size      = "m",
      easyClose = TRUE
    ))
  })
  
  # Render the QR code for the standalone modal (distinct output so the
  # two modals can coexist if somehow both were opened).
  output$qr_code_plot_standalone <- renderPlot({
    qr <- qrcode::qr_code("https://vote.sos.ri.gov/Home/RegistertoVote")
    plot(qr)
  }, bg = "transparent")
  # ── Save Add Info (DB) ──────────────────────────────────────────────────
  observeEvent(input$save_addl_info, {
    v <- current_voter()
    if (is.null(v)) return()
    vid <- as.character(v$VoterId)
    
    user    <- tolower(credentials()$info$user)
    initial <- who_initials[user] %||% toupper(substr(user, 1, 1))
    know_them_val <- validate_choice(input$addl_know_them, c("Y", "N"))
    who_val <- if (isTRUE(know_them_val=="Y")) initial else NA_character_
    
    # This is the user's input for clicking "Additional Info?" and writing additional things.
    # Set it to "" if it's blank.
    raw_text <- input$addl_text %||% ""
    text_val  <- sanitize_user_input(raw_text)
    # Clean the raw_text at a basic level. 
    # First - set raw_text to as.character(). 
    # Then - trim whitespace.
    # Then - gsub specific things ("control chars"?) x <- gsub("[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]", "", x, perl = TRUE).
    # Then - 
    
    # Check db_helpers.R insert_additional_info() for next step. 
    # Audit row + who_knows merge on voters
    insert_additional_info(
      con,
      vid             = vid,
      who_knows       = who_val,
      additional_text = text_val,
      updated_by      = credentials()$info$user
    )
    
    # Persist the initial on voters.who_knows so it survives session
    # restart. append_who_knows dedupes — safe to call repeatedly.
    if (!is.na(who_val)) {
      append_who_knows(con, vid, who_val)
    }
    
    # Update in-memory voters_raw for WhoKnows only.
    # Additional notes are now read live from additional_info table
    # at display time — no need to update voters_raw.
    
    vid_match <- voters_raw$VoterId == v$VoterId
    if (isTRUE(know_them_val == "Y")) {
      existing_who <- voters_raw$WhoKnows[vid_match]
      existing_who <- if (is.na(existing_who) || existing_who == "") "" else existing_who
      all_initials <- unique(unlist(strsplit(paste(na.omit(c(existing_who, initial)), collapse = ", "), ", ")))
      all_initials <- all_initials[all_initials != ""]
      voters_raw$WhoKnows[vid_match] <<- paste(all_initials, collapse = ", ")
    }
    
    addl_support_val <- validate_choice(input$addl_support, validators$support_rating_choice)
    if (!is.na(addl_support_val)) {
      insert_contact(con, vid = vid,
                     input_by = credentials()$info$user,
                     fields = list(support_rating = addl_support_val))
    }
    
    data_refresh(data_refresh() + 1)
    
    removeModal()
    showNotification("Info saved!", type = "message")
  })
  
  invisible(NULL)
}
