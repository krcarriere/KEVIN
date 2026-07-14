# Testing

library(shiny)
library(bslib)
library(DT)
library(shinyauthr)
if ("package:geosphere" %in% search()) detach("package:geosphere", unload = TRUE)

login_path <- "Functions/logins.R"
password_mgmt_path <- "Modules/password_management.R"
security_path <- "Functions/security.R"
helper_functions_path <- "Functions/db_helpers.R"

db_path    <- "ServerData/kevin.sqlite"
config_path <- "Functions/config.R"
gen_maps_links_path <- "Functions/google_maps_link.R"
general_helpers_path <- "Functions/general_helpers.R"

canvassing_module_path <- "Modules/canvassing_scripts.R"
voter_contact_modals_path <- "Modules/voter_contact_modals.R"
voter_list_voterbase_path <- "Modules/voter_list_voterbase.R"

variable_list_path <- "Functions/variables_included.R"

# We're removing results for optimization purposes for now.
# 5/16/26
# results_graph_path <- "Modules/concerns_results.R"

source(login_path)
# load_user_base(con)
source(config_path)
# user_campaigner
# who_initials
# concern_choices
# win_targets
# scenario_access
source(security_path)
# sanitize_user_input

source(helper_functions_path)
# open_db
# close_db
# latest_contact
# latest_field_map
# voter_scenario
# voters_off_route
# voters_with_any_contact
# insert_contact
# flag_may_have_moved
# insert_new_contact
# insert_additional_info
# append_who_knows
# append_additional
# get_additional_notes
# validate_choice
# get_same_door_voters

source(gen_maps_links_path)
# build_direction_links
source(general_helpers_path)
# dated_col
source(canvassing_module_path)
#  registerVoterContactModals
source(voter_contact_modals_path)
#  observeEvent(input$diff_person
#  observeEvent(input$diff_person_yes,
#  observeEvent(input$diff_person_no, 
#  output$qr_code_plot
#  observeEvent(input$diff_person_continue
#  observeEvent(input$save_new_contact
#  output$new_concern1_other_ui
#  output$new_concern2_other_ui
#  output$new_concern3_other_ui
#  observeEvent(input$add_info,
#  observeEvent(input$save_addl_info,
source(voter_list_voterbase_path)
# voterListVoterBaseServer()
# voterListVoterBaseTableUI()
# voterListVoterBaseFiltersUI()

source(variable_list_path)
source(password_mgmt_path)

# Removed 5/19/26.
#source(results_graph_path)

# ── Load field data (app-owned, separate from pipeline data) ──
# ── Connect to the DB, load voters + routes into memory ──────────────────
# voters_raw and routes_raw stay as in-memory data frames (Path A) for
# read-cheap access. Contacts and additional_info are queried live from
# the DB via helpers in db_helpers.R — they change during a session.
con <- open_db(db_path)
user_base <- load_user_base(con)

voters_raw <- dplyr::tbl(con, "voters") |> dplyr::collect()
routes_raw <- dplyr::tbl(con, "routes") |> dplyr::collect()

# Bring column names back to the shape the app's downstream code expects
# (CamelCase to match the CSV-era convention). This is a one-spot
# translation so the rest of the app code doesn't need to change.
voters_raw <- voters_raw |>
  dplyr::rename(
    VoterId          = voter_id,
    LastName         = last_name,
    FirstName        = first_name,
    MiddleName       = middle_name,
    FullName         = full_name,
    StreetNumber     = street_number,
    StreetName       = street_name,
    SuffixA          = suffix_a,
    SuffixB          = suffix_b,
    Unit             = unit,
    UnitNum          = unit_num,
    ZipCode          = zip_code,
    Zip4Code         = zip4_code,
    ZeroZipCode      = zero_zip_code,
    City             = city,
    State            = state,
    FullAddress      = full_address,
    Street           = street,
    PartyCode        = party_code,
    EffectiveDate    = effective_date,
    Gender           = gender,
    RegistrationDate = registration_date,
    DateMadeDateInactive     = date_made_inactive,
    StateSenateStateDistrict = state_senate_district,
    StateRepStateDistrict    = state_rep_district,
    Precinct         = precinct,
    Ward.council     = ward_council,
    WardDistrict     = ward_district,
    generalScore     = general_score,
    primaryScore     = primary_score,
    TurnoutScore     = turnout_score,
    Primary2024      = primary_2024,
    Primary2022      = primary_2022,
    Primary2020      = primary_2020,
    General2024      = general_2024,
    General2022      = general_2022,
    General2020      = general_2020,
    VoterStatus      = voter_status,
    Age              = age,
    Lat              = lat,
    Long             = long,
    TimesSpoken      = times_spoken,
    nsse_2026_voter  = nsse_2026_voter,
    DonateText       = donate_text,
    Additional       = additional,
    WhoKnows         = who_knows,
    MayHaveMoved     = may_have_moved
  )

routes_raw <- routes_raw |>
  dplyr::rename(
    VoterId          = voter_id,
    Scenario         = scenario,
    Precinct         = precinct,
    FullAddress      = full_address,
    VotersAtAddress  = voters_at_address,
    Route            = route,
    StopOrder        = stop_order,
    RouteName        = route_name
  )

# Runtime mutations (same as CSV-era): compute Name, Street, StreetNameUpper
# etc. that downstream code depends on. Note: WhoKnows already came from
# the DB (migration baked the initials-mapping), so we skip the case_when
# that did that translation before.
voters_raw <- voters_raw |>
  dplyr::mutate(
    StreetSuffix      = stringr::str_extract(FullAddress, "\\S+(?=,)"),
    StreetName        = stringr::str_match(FullAddress, "^\\d+\\s([^,]+)")[, 2],
    StreetName        = stringr::str_remove(StreetName, StreetSuffix),
    StreetName        = stringr::str_to_title(StreetName),
    FullAddress       = stringr::str_to_title(FullAddress),
    FullAddress = stringr::str_replace(FullAddress, "\\bRi\\b", "RI"),
    StreetNum         = as.integer(stringr::str_match(FullAddress, "^\\s*(\\d+)")[, 2]),
    Name              = FullName,
    Street            = FullAddress,
    Party             = PartyCode,
    VoterScore        = TurnoutScore,
    StreetSuffixUpper = toupper(trimws(StreetSuffix)),
    StreetNameUpper   = toupper(trimws(StreetName)),
    RowID             = dplyr::row_number()
  )

# LastName / FirstName parsing fallback (unchanged from before)
if (!"LastName" %in% names(voters_raw) || all(is.na(voters_raw$LastName))) {
  if (any(grepl(",", voters_raw$FullName, fixed = TRUE), na.rm = TRUE)) {
    voters_raw$LastName  <- trimws(sub(",.*", "", voters_raw$FullName))
    voters_raw$FirstName <- trimws(sub(".*,\\s*", "", voters_raw$FullName))
  } else {
    voters_raw$FirstName <- trimws(sub("\\s+\\S+$", "", voters_raw$FullName))
    voters_raw$LastName  <- trimws(sub(".*\\s+", "", voters_raw$FullName))
  }
}
voters_raw$LastNameUpper  <- toupper(trimws(voters_raw$LastName))
voters_raw$FirstNameUpper <- toupper(trimws(voters_raw$FirstName))


assign("voters_raw", voters_raw, envir = globalenv())
assign("routes_raw", routes_raw, envir = globalenv())

# ── UI ──────────────────────────────────────────────────────────────────────
ui <- page_fluid(
  theme = bs_theme(
    bg        = "#FFFFFF",     # warm cream background
    fg        = "#3A4555",     # dark blue text
    primary   = "#3A4555",     # primary buttons
    secondary = "#729AB9",     # accent elements
    success   = "#3A4555",     # save/success badges
    danger    = "#6E120B",     # red accents
    "sidebar-bg"   = "#3A4555",
    "sidebar-fg"   = "#FBF1D8",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro")
  ),
  # "A small heartbeat"
  tags$head(
    tags$script(HTML("
    setInterval(function() {
      Shiny.setInputValue('keepalive', new Date().getTime());
    }, 15000);
  "))
  ),
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  tags$head(tags$script(src = "inactivity.js")), 
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  tags$head(tags$script(src = "inactivity.js")), 
  tags$head(tags$script(HTML("
  document.addEventListener('shiny:connected', function() {
    setTimeout(function() {
      var userInput = document.querySelector('input[id$=\"user_name\"]');
      if (userInput) {
        userInput.setAttribute('autocapitalize', 'none');
        userInput.setAttribute('autocorrect', 'off');
        userInput.setAttribute('spellcheck', 'false');
      }
    }, 100);
  });
                             "))),
  # Logo + login
  div(class = "login-logo",
      tags$img(src = "DTC_Logo_B_dark_blue_and_red.png", alt = "NS DTC")),
  loginUI(id = "login"),
  shinyjs::useShinyjs(),
  shinyjs::hidden(
    div(id = "app_content",
        page_sidebar(
          sidebar = sidebar(
            width = 320,
            selectInput("scenario", "Voter Base",
                        choices = c("Select a set of voters" = "", 
                                    "NS Democrats" = "NSDem",
                                    "NS Independents" = "NSInd",
                                    "NS Dem + Ind" = "NSDandI",
                                    "N.S. Voters" = "NSAll",
                                    "D17 Democrats" = "D17Dem",
                                    "D17 Independents" = "D17Ind",
                                    "D17 Dem + Ind" = "D17DandI",
                                    "Kevin Only" = "KevinOnly")),
            accordion(
              id = "search_mode",
              open = FALSE,
              # Other option is: 
              #"Search by Route #",
              
              accordion_panel(
                "Search by Route #",
                icon = bsicons::bs_icon("signpost-split"),
                selectInput("precinct", "What Precinct?", choices = NULL),
                selectInput("route_num", "What Route?", choices = NULL),
                uiOutput("print_route_link"),
                actionButton("go_route", "Search", class = "btn-primary w-100 mt-2")
              ),
              
              accordion_panel(
                "Search by Street",
                icon = bsicons::bs_icon("geo-alt"),
                numericInput("house_num", "House number (optional)", value = NA, min = 1),
                textInput("street_search", "Street name", placeholder = "e.g. MAPLE"),
                selectInput("street_match", "Street", choices = c("(type above)" = "")),
                actionButton("go_street_all", "Who's on this street?", class = "btn-secondary w-100 mt-2"),
                actionButton("go_street_route", "Find route by house #", class = "btn-primary w-100 mt-2")
              ),
              
              accordion_panel(
                "Search by Name",
                icon = bsicons::bs_icon("person"),
                textInput("search_last", "Last name", placeholder = "e.g. SMITH"),
                selectizeInput("search_first", "Full name",
                               choices = NULL,  # populated server-side
                               options = list(
                                 placeholder = "(type last name first)",
                                 maxOptions = 100  # cap visible options for perf
                               )),
                actionButton("go_name", "Search", class = "btn-primary w-100 mt-2")
              ),
              
              accordion_panel(
                "Additional Filters",
                icon = bsicons::bs_icon("funnel"),
                tags$small(
                  style = "color: #666;",
                  "After changing filters, click search above again."
                ),
                checkboxInput("filter_history", "Filter by voting history?", value = FALSE),
                conditionalPanel(
                  condition = "input.filter_history",
                  checkboxGroupInput(
                    "history_elections",
                    "Require 'Voted' in:",
                    choices = setNames(
                      validators$history_elections,
                      names(history_map)
                    )
                  )
                ),
                checkboxInput("filter_special", "Filter by 🚔 Voter?", value = FALSE),
                conditionalPanel(
                  condition = "input.filter_special",
                  checkboxGroupInput(
                    "specialvote_levels",
                    "Include:",
                    choices = c(
                      "Yes (Voted)" = "Yes",
                      "No (Did not vote)" = "No"
                    ),
                    selected = c("Yes", "No")
                  )
                ),
                
                checkboxInput("filter_age", "Filter by age?", value = FALSE),
                conditionalPanel(
                  condition = "input.filter_age",
                  numericInput("age_min", "Minimum age:", value = 65, min = 0, max = 120, step = 1)
                ),
                checkboxInput("include_may_have_moved",
                              "Include 'May have moved'?",
                              value = TRUE),
                
                checkboxInput("include_inactive_voters",
                              "Include 'Inactive Voters'?",
                              value = TRUE)
              ),
              tags$hr(),
              password_change_button_UI("pw_mod")
            )
          ),
          
          navset_tab(
            id = "main_tabs",
            # ── Tab 1: Record Canvass ───────────────────────────────────────────
            nav_panel(
              value = "contact_card",
              "Record Contact",
              card(
                card_header(
                  class = "d-flex justify-content-between align-items-center",
                  span(textOutput("contact_header", inline = TRUE),
                       actionLink("try_another",
                                  label = tags$small(style = "color: #FBF1D8;",
                                                     bsicons::bs_icon("arrow-clockwise"), 
                                                     " Different Route"))),
                  span(class = "d-flex align-items-center",
                       actionButton("prev_stop", "\u25C0 Prev", class = "btn-sm btn-outline-secondary me-1"),
                       actionButton("next_stop", "Next \u25B6", class = "btn-sm btn-outline-secondary me-2"),
                       div(class = "me-1", style = "width: 4.5rem; margin-bottom: 0;",
                           numericInput("jump_stop", label = NULL, value = NA, min = 1)),
                       actionButton("go_stop", "Go", class = "btn-sm btn-outline-secondary")
                  )
                ),
                layout_columns(
                  col_widths = c(4, 8),
                  
                  # Left: voter info
                  card(
                    class = "bg-light",
                    card_body(
                      uiOutput("voter_info_panel")
                    )
                  ),
                  
                  # Right: server-rendered form
                  card(
                    card_body(
                      uiOutput("canvass_form")
                    )
                  )
                )
              )
            ),
            
            # ── Tab 2: Voter List ────────────────────────────────────────────────────
            nav_panel(
              value = "voter_list",
              "Voter List",
              card(
                card_header(textOutput("voter_list_header")),
                layout_columns(
                  col_widths = c(3, 9),
                  
                  card(
                    class = "bg-light",
                    card_body(
                      tags$b("View"),
                      radioButtons("voter_list_mode", NULL,
                                   choices = c("Route" = "route",
                                               "Voter Base" = "voterbase"),
                                   selected = "route", inline = TRUE),
                      tags$hr(),
                      
                      conditionalPanel(
                        condition = "input.voter_list_mode == 'voterbase'",
                        voterListVoterBaseFiltersUI("voter_base_mod")
                      ),
                      conditionalPanel(
                        condition = "input.voter_list_mode == 'route'",
                        tags$p(class = "text-muted small",
                               "Route view shows results from sidebar searches.")
                      )
                    )
                  ),
                  
                  card(
                    card_body(
                      uiOutput("voter_list_results")
                    )
                  )
                )
              )
            ),
            # ── Tab 3: Directions ───────────────────────────────────────────────
            nav_panel(
              value = "Directions",
              "Directions",
              card(
                card_header("Walking Directions"),
                checkboxInput("use_phone", "Using Phone?", value = FALSE),
                uiOutput("direction_links")
              )
            ),
            # --- Script 
            nav_panel(
              value = "canvass_script",
              "Script",
              icon = bsicons::bs_icon("chat-quote"),
              card(
                card_header("Canvassing Script"),
                card_body(
                  scriptUI("script_mod")
                )
              )
            ),
            # My Maps -- Removed 5/19/26 
            # Embed does not work with security. Revisit later.
            # nav_panel(
            #   value = "precinct_map",
            #   "Map",
            #   icon = bsicons::bs_icon("map"),
            #   card(
            #     card_header("Route Maps"),
            #     card_body(
            #       uiOutput("map_embed")
            #     )
            #   )
            # ),
            # Results Tab
            # Removed 5/19/26
            # Hits hard on load. Revisit to new site.
            # nav_panel(
            #   value = "knock_results",
            #   "Results",
            #           icon = bsicons::bs_icon("bar-chart-fill"),
            #           concernsResultsUI("concerns_mod")
            # ),
            nav_panel(
              value = "faq",
              "FAQ",
              icon = bsicons::bs_icon("question-circle"),
              card(
                card_header("Quick Guide"),
                card_body(
                  tags$div(style = "max-width: 720px;",
                           
                           # ── Icons section ─────────────────────────────────────
                           tags$h5("What do the icons mean?"),
                           tags$p(tags$em("Icons appear next to a voter's name on the contact card.")),
                           tags$ul(
                             tags$li(
                               tags$span(style = "color: #6E120B;",
                                         bsicons::bs_icon("exclamation-triangle-fill")),
                               tags$strong(" Warning triangle"),
                               " — This voter is marked ",
                               tags$strong("inactive"), 
                               " on the state voter roll. They may have missed elections or have moved;",
                               "Try and get them to register to vote."
                             ),
                             tags$li(
                               tags$span("\U0001F4E6"),
                               tags$strong(" Moving box"),
                               " — We think this person ",
                               tags$strong("may have moved"), 
                               ". Someone was recorded at this door who wasn't one of the listed residents."
                             ),
                             tags$li(
                               tags$span(style = "color: #6E120B;", "\U0001F4B2"),
                               tags$strong(" Dollar sign"),
                               " — this voter has ",
                               tags$strong("donated"),
                               " to a federal or state campaign recently. Say thank you!"
                             ),
                             tags$li(
                               tags$span("\U0001F3A4"),
                               tags$strong(" Microphone"),
                               " — this voter has ",
                               tags$strong("spoken at town council"),
                               " meetings. The number indicates how many times."
                             ),
                             tags$li(
                               tags$span(class = "badge",
                                         style = "background-color: #6E120B; color: #FFFFFF;",
                                         "P / R / K"),
                               tags$strong(" Candidate badge"),
                               " — this voter has a ",
                               tags$strong("personal connection"),
                               " to one or more candidates. ",
                               "P = Paul, K = Kevin. Combinations like 'P/K' mean multiple connections."
                             )
                           ),
                           
                           tags$hr(),
                           
                           # ── Sidebar section ──────────────────────────────────
                           tags$h5("Using the sidebar"),
                           tags$p("The sidebar on the left has four main sections:"),
                           tags$ul(
                             tags$li(tags$strong("Scenario"), 
                                     " — who you're canvassing today."),
                             tags$li(tags$strong("By Route"),
                                     " — pick a precinct and route number to load a pre-built walking route."),
                             tags$li(tags$strong("By Address / Name"),
                                     " — look up a specific voter by street, last name, or first name. ",
                                     "After matching, click 'Find route' to load the route containing that person."),
                             tags$li(tags$strong("Additional Filters"),
                                     " — narrow down the route:"),
                             tags$ul(
                               tags$li(tags$em("By voting history"),
                                       " — show only voters who voted in specific recent elections."),
                               tags$li(tags$em("By stated support"),
                                       " — show only voters with specific support ratings from prior canvasses. ",
                                       "Check 'Show List?' to flip to a sortable table view."),
                               tags$li(tags$em("By party"),
                                       " — show only certain party affiliations."),
                               tags$li(tags$em("Include 'May have moved?'"),
                                       " — by default these are shown; uncheck to hide them.")
                             )
                           ),
                           
                           tags$hr(),
                           
                           # ── Workflow section ─────────────────────────────────
                           tags$h5("Basic canvassing workflow"),
                           tags$ol(
                             tags$li("Pick your ", tags$strong("scenario"), 
                                     " (or leave it on the default for your role)."),
                             tags$li("Load a ", tags$strong("route"),
                                     " by precinct + route number, or search by address or name."),
                             tags$li("Walk to the first stop. The contact card at the top shows the voter's info."),
                             tags$li("Fill out ", tags$strong("Knocking for who?"), ", ",
                                     tags$strong("Left material?"), ", and ",
                                     tags$strong("Were they there?"), "."),
                             tags$li("If they were there, record ", tags$strong("support rating"),
                                     " and any ", tags$strong("concerns"), " that came up."),
                             tags$li("Click ", tags$strong("Save & Next"), " to move on."),
                             tags$li("When the whole route is done, use the ",
                                     tags$strong("Directions"), " tab to walk back to your car.")
                           ),
                           
                           tags$hr(),
                           
                           # ── Special buttons ──────────────────────────────────
                           tags$h5("Special buttons on the contact card"),
                           tags$ul(
                             tags$li(
                               tags$span(class = "badge bg-danger", "Different Person"),
                               " — use when the person at the door ",
                               tags$strong("isn't on the voter roll"),
                               " (a new resident, roommate, family member). ",
                               "You'll be shown a list of registered voters at that address in case it's one of them. ",
                               "If not, you can add them as a new contact."
                             ),
                             tags$li(
                               tags$span(class = "badge bg-primary",
                                         bsicons::bs_icon("plus-circle-fill"), " Add Info"),
                               " — add a note about this voter (things you know about them, ",
                               "personal connections, or update a support rating later)."
                             )
                           ),
                           
                           tags$hr(),
                           
                           # ── Troubleshooting ──────────────────────────────────
                           tags$h5("Troubleshooting"),
                           tags$ul(
                             tags$li(tags$strong("I don't see a voter I just added."),
                                     " Make sure you're on the same route as where you added them. ",
                                     "They'll appear at the same stop as the door you knocked on."),
                             tags$li(tags$strong("The progress bar shows 0."),
                                     " The counts only populate once you've started recording contacts. ",
                                     "DTC members are pre-counted as supporters."),
                             tags$li(tags$strong("Something looks wrong or crashed."),
                                     " Tell Kevin what you were doing, and include any red error messages you saw.")
                           ),
                           
                           # ── Changelog ──────────────────────────────────
                           tags$h5("Changelog"),
                           tags$h6("April 25th"),
                           tags$ul(
                             tags$li(tags$strong("Generate routes."),
                                     "Added ability to have routes generate for you. Routes are ",
                                     "presented in order of how valuable the routes are for a given voterbase, and how little of the route we have already canvassed.")
                             
                           
                  )
                  )
                )
              )
            )
          ), 
          uiOutput("vote_progress"),
          tags$div(
            class = "text-center text-muted",
            style = "margin-top: 0.5rem; font-size: 0.85em;",
            "Knowledge and Engagement Voter Information Network (KEVIN App). 
            Last updated: April 25, 2026. 
    Email krcarriere[a t]gmail with bugs or questions."
          )
        )
    )))

# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # -- Additional Security Adds? --------------------------------------
  
  # 
  session$allowReconnect("force")
  
  observeEvent(input$keepalive, {}, ignoreNULL = TRUE)
  
  # ── Authentication ─────────────────────────────────────────────────
  #user_base_rv <- reactiveVal(user_base)
  # Doesnt seem used - 4/27/26.
  
  credentials <- loginServer(
    id = "login",
    data = user_base,
    user_col = tolower("user"),
    pwd_col = "password",
    sodium_hashed = TRUE,
    log_out = reactiveVal(FALSE)
  )
  
  user_role <- reactive({
    req(credentials()$user_auth)
    user_base$role[user_base$user == credentials()$info$user]
  })
  
  user_scenarios <- reactive({
    scenario_access[[user_role()]]
  })
  
  # Voters the user is allowed to search across by name, regardless of
  # which scenario is currently selected. Scoped by role: an NS user
  # sees all North Smithfield voters, a D17 user stays in D17, a
  # Pawtucket user stays in Pawtucket. This is wider than voter_data()
  # (which is scenario-specific) but narrower than voters_raw (which
  # spans all jurisdictions loaded into the DB).
  searchable_voters <- reactive({
    req(credentials()$user_auth)
    data_refresh()
    allowed_precincts <- routes_raw |>
      dplyr::filter(Scenario %in% user_scenarios()) |>
      dplyr::distinct(Precinct) |>
      dplyr::pull(Precinct)
    voters_raw |> dplyr::filter(Precinct %in% allowed_precincts)
  })
  
  shiny::observeEvent(credentials()$user_auth, {
    req(credentials()$user_auth)
    shinyjs::show("app_content")
    shinyjs::hide("login-panel")  # hide login form
    
    # Update scenario choices for this user's role
    all_labels <- c("Select a set of voters" = "", 
                    "NS Democrats" = "NSDem",
                    "NS Independents" = "NSInd",
                    "NS Dem + Ind" = "NSDandI",
                    "N.S. Voters" = "NSAll",
                    "D17 Democrats" = "D17Dem",
                    "D17 Independents" = "D17Ind",
                    "D17 Dem + Ind" = "D17DandI",
                    "Kevin's Only" = "KevinOnly",
                    "K or Donor" = "KevinOrDonor",
                    "D49 Democrats" = "D49Dem",
                    "D49 Independents" = "D49Ind",
                    "D49 Dem + Ind" = "D49DandI",
                    
                    "Pawtucket" = "Pawtucket")
    
    #my_scenarios <- all_labels[all_labels %in% user_scenarios()]
    # Removed 4-27-26.
    allowed <- intersect(user_scenarios(), unname(all_labels))
    my_scenarios <- all_labels[all_labels %in% allowed]
    
    my_scenarios <- c("Select a set of voters" = "", my_scenarios)
    default <- switch(user_role(),
                      "ns"    = "NSDandI",
                      "sb"    = "NSDem",
                      "admin" = "NSDandI",
                      "d17"   = "D17Dem",
                      "d49"   = "D49Dem",
                      "paw"   = "Pawtucket",
                      "")
    
    updateSelectInput(session, "scenario", choices = my_scenarios, selected = default)
  }, ignoreInit = TRUE)
  
  
  #----- Data -------
  # field_data is kept as an empty reactiveVal for the Phase 2 transition.
  # Downstream code that reads it will be replaced in the next round. For
  # now, reads return empty and most downstream code short-circuits
  # gracefully on nrow(fd) == 0 checks.
  field_data  <- reactiveVal(data.frame(VoterId = character(0)))
  route_data  <- reactiveVal(tibble::tibble())
  header_text <- reactiveVal("Select a search mode and click Search.")
  current_stop <- reactiveVal(1L)
  form_phase  <- reactiveVal(1L)
  data_refresh <- reactiveVal(0)
  talked_to_ids    <- reactiveVal(NULL)   # NULL = not chosen yet; character vector once chosen
  person_queue_idx <- reactiveVal(1L)     # position within talked_to_ids being answered
  person_answers   <- reactiveVal(list()) # vid -> list(support_rating, concern1-3, concern1-3_other, contact_addl)
  
  # Defensive reset: whenever we move to a different stop, blank out the
  # conversation-dependent inputs (had_conversation, support_rating,
  # concern1-3 and their "Other" text). These widgets only get re-rendered
  # by voter_info_panel when a stop's Phase 3 is actually reached — if the
  # next stop skips straight to Phase 4 (no answer / no conversation), the
  # previous stop's stale selections would otherwise still be sitting in
  # the browser's input bindings. do_save() already gates on full_convo so
  # this can't leak into the DB, but resetting here keeps the UI itself
  # from ever showing a prior stop's picks.
  observeEvent(current_stop(), {
    updateRadioButtons(session, "had_conversation", selected = character(0))
    updateSelectInput(session, "support_rating", selected = "")
    updateSelectInput(session, "concern1", selected = "(none)")
    updateSelectInput(session, "concern2", selected = "(none)")
    updateSelectInput(session, "concern3", selected = "(none)")
    updateTextInput(session, "concern1_other", value = "")
    updateTextInput(session, "concern2_other", value = "")
    updateTextInput(session, "concern3_other", value = "")
    updateTextInput(session, "contact_addl", value = "")
    talked_to_ids(NULL)
    person_queue_idx(1L)
    person_answers(list())
  }, ignoreInit = TRUE)
  
  # Joined voter+route data for the selected scenario
  # This is the key workhorse. 
  voter_data <- reactive({
    req(credentials()$user_auth)
    req(input$scenario != "")
    # Server-side authorization: even if a malicious authenticated user
    # manually sets input$scenario via the browser console to a scenario
    # they shouldn't see, this stops data from being sent to them.
    req(input$scenario %in% user_scenarios())
    
    voter_scenario(con, input$scenario)
    
    data_refresh()  # triggers re-evaluation when bumped
    scen <- input$scenario
    scen_routes <- routes_raw %>% dplyr::filter(Scenario == scen)
    
    main <- voters_raw %>%
      dplyr::inner_join(scen_routes, by = c("VoterId", "Precinct")) |>
      dplyr::select(-starts_with("X")) |>
      dplyr::mutate(OnTargetList = TRUE)
    
    scen_precincts <- unique(scen_routes$Precinct)
    address_route_map <- scen_routes %>%
      distinct(Precinct, FullAddress, Route, StopOrder, RouteName, Scenario)
    
    eligible_voters <- voters_raw %>%
      filter(
        VoterId %in% main$VoterId |
          paste(Precinct, Street) %in%
          paste(address_route_map$Precinct, address_route_map$FullAddress)
      )
    
    off_route <- 
      eligible_voters %>%
      filter(!(VoterId %in% main$VoterId)) %>%
      left_join(address_route_map,
                by = c("Precinct" = "Precinct",
                       "Street"   = "FullAddress")) %>%
      mutate(
        Scenario = scen,
        OnTargetList = FALSE
      )
    out <- dplyr::bind_rows(main, off_route)
    
    # May-have-moved filter: hide voters flagged as may_have_moved, unless
    # the user opts to include them. The flag is a voter-level column now
    # (baked in by migration, updated by save handlers).
    # Because this is binary T/F, I do not need intersection.
    if (!isTRUE(input$include_may_have_moved)) {
      out <- out |> dplyr::filter(MayHaveMoved == 0 | is.na(MayHaveMoved))
    }
    
    # Inactive Voters filter: hide voters flagged inactive.
    # Because this is binary T/F, I do not need intersection.
    if (!isTRUE(input$include_inactive_voters)) {
      out <- out |>
        dplyr::filter(is.na(VoterStatus) | VoterStatus != "Inactive")
    }
    
    # Voting History Filter
    # This design is more secure because I'm using the hard-coded column names in config.R.
    # Uses a strict whitelist (validators$history_elections) as the source of truth.
    # Input is intersected with allowed values to prevent invalid or injected columns.
    # Then applies an AND constraint: all selected election columns must equal "Voted".
    
    # UI is also controlled via intersect, but that's visual. 
    if (isTRUE(input$filter_history) && length(input$history_elections) > 0) {
      cols <- intersect(input$history_elections, validators$history_elections)
      if (length(cols) > 0) {
        out <- out |>
          dplyr::filter(if_all(all_of(cols), ~ .x == "Voted"))
      }
    }
    
    # support rating filter
    # LatestSupport: pull latest support_rating per voter from the DB.
    # DTC voters count as "1" (strong support) by definition — merged in
    # below so the filter treats them correctly.
    support_lookup <- latest_field_map(con, "support_rating")
    
    out <- out |>
      dplyr::mutate(
        LatestSupport = unname(support_lookup[as.character(VoterId)]),
        # DTC voters → "1" if no contact rating yet
        LatestSupport = dplyr::if_else(
          (is.na(LatestSupport) | LatestSupport == "") & 
            !is.na(is_dtc) & is_dtc == 1,
          "1",
          LatestSupport
        )
      )
    
    #SpecialElectionFilter
    if (isTRUE(input$filter_special) && length(input$specialvote_levels) > 0) {
      out <- out |>
        dplyr::filter(
          nsse_2026_voter %in% input$specialvote_levels |
            (is.na(nsse_2026_voter) & "NA" %in% input$specialvote_levels)
        )
    }
    
    # Age filter: voters at or above the minimum. NA ages are kept
    # (new contacts, mostly) — matching the same principle as the party
    # filter keeping unknown-party voters visible.
    if (isTRUE(input$filter_age) && !is.null(input$age_min) && !is.na(input$age_min)) {
      out <- out |> dplyr::filter(is.na(Age) | Age >= input$age_min)
    }
    
    # Per-scenario address trim — strips "..., North Smithfield, RI, 02896"
    # off the end for NS scenarios. Keeps the original Street/FullAddress
    # untouched so Directions can use the full address for Google Maps.
    trim_pattern <- address_trim[[scen]]
    if (!is.null(trim_pattern)) {
      out <- out |>
        dplyr::mutate(
          DisplayAddress = stringr::str_remove(Street, stringr::fixed(trim_pattern))
        )
    } else {
      out <- out |> dplyr::mutate(DisplayAddress = Street)
    }
    
    # ── Browser-visible columns allowlist ──────────────────────────────────
    # voter_data() feeds every reactive that sends voter info to the client.
    # An explicit allowlist here means that adding a new column upstream
    # (in the pipeline or in voters_raw startup prep) does NOT automatically
    # make that column browser-visible. If a new field needs to reach the
    # UI, add it here — a single audit point.
    #
    # any_of() silently skips columns that don't exist, so we don't crash
    # when the pipeline temporarily drops a column.
    
    out <- out |>
      dplyr::select(dplyr::any_of(c(variable_list)))
    
    out
    
  })
  
  # Track which "phase" of the form we're in:
  #   1 = base questions (campaigner, left material, were they there)
  #   2 = conversation follow-up (had conversation? + support + concerns)
  #   3 = done / ready to save (they weren't there, or form complete)
  
  
  # Intro Score Route
  # Index for cycling through suggestions ("Try another")
  suggestion_idx <- reactiveVal(1L)
  
  # Reset to top suggestion when scenario changes
  observeEvent(input$scenario, {
    suggestion_idx(1L)
  })
  
  suggested_routes <- reactive({
    req(credentials()$user_auth)
    req(input$scenario != "")
    data_refresh()
    
    user_precinct <- user_base$precinct[user_base$user == credentials()$info$user]
    
    # Score every voter in the current scenario
    v <- voter_data() |>
      dplyr::mutate(
        VS_norm = pmin(1, VoterScore / 3.75, na.rm = TRUE),
        nsse_score = dplyr::case_when(
          nsse_2026_voter == "Yes" ~ 1,
          nsse_2026_voter == "No"  ~ 0,
          TRUE                     ~ 0.25
        ),
        party_score = dplyr::case_when(
          Party == "D" ~ 1,
          Party == "U" ~ 0.65,
          Party == "R" ~ 0.2,
          TRUE         ~ 0.5
        ),
        whoknows_score = ifelse(!is.na(WhoKnows) & WhoKnows != "", 1, 0),
        value = 0.35 * VS_norm + 0.15 * nsse_score +
          0.30 * party_score + 0.20 * whoknows_score
      )
    
    contacted_ids <- voters_with_any_contact(con)
    
    routes_scored <- v |>
      dplyr::filter(!is.na(Route), OnTargetList) |>
      dplyr::group_by(Precinct, Route, RouteName) |>
      dplyr::summarize(
        n_voters      = dplyr::n(),
        avg_value     = mean(value, na.rm = TRUE),
        pct_contacted = mean(as.character(VoterId) %in% contacted_ids),
        .groups       = "drop"
      ) |>
      # Hard exclusions: apartment complexes (inflated by lat/long collisions
      # in the pipeline; revisit threshold if pipeline ever rebuilds with
      # unit-aware stop counts) and routes mostly canvassed already.
      dplyr::filter(
        n_voters <= 50,
        pct_contacted < 0.7
      ) |>
      # Soft precinct preference: own-precinct routes score higher, but
      # other-precinct routes are still eligible if the user has finished
      # their precinct or doesn't have one set.
      dplyr::mutate(
        is_own_precinct = !is.na(user_precinct) & 
          as.character(Precinct) == as.character(user_precinct),
        sort_value = avg_value + ifelse(is_own_precinct, 0.10, 0)
      ) |>
      dplyr::arrange(dplyr::desc(sort_value))
    
    routes_scored
  })
  
  
  load_suggested_route <- function(idx = suggestion_idx()) {
    candidates <- suggested_routes()
    idx <- suggestion_idx()
    
    if (nrow(candidates) == 0) {
      showNotification("No suggested routes available — pick one manually from the sidebar.",
                       type = "warning", duration = 6)
      return()
    }
    
    if (idx > nrow(candidates)) {
      showNotification("That's all the suggestions — pick one manually from the sidebar.",
                       type = "message", duration = 5)
      return()
    }
    
    pick <- candidates[idx, ]
    
    df <- voter_data() |>
      dplyr::filter(Precinct == pick$Precinct, Route == pick$Route) |>
      dplyr::arrange(StopOrder)
    
    load_route(df)
    
    precinct_note <- if (isTRUE(pick$is_own_precinct)) "" else " (outside your precinct)"
    header_text(paste0("Suggested: Precinct ", pick$Precinct, " — ",
                       pick$RouteName, "  (", pick$n_voters, " voters, ",
                       round(pick$pct_contacted * 100), "% knocked",
                       precinct_note, ")"))
  }
  
  observeEvent(input$suggest_route, {
    suggestion_idx(1L)
    load_suggested_route(idx = 1L)  # explicit, not reactive read
  })
  
  observeEvent(input$try_another, {
    new_idx <- suggestion_idx() + 1L
    suggestion_idx(new_idx)
    load_suggested_route(idx = new_idx)
  })
  
  # ── Searching ──────────────────────────────────────────────────────────
  
  load_route <- function(df) {
    
    if (nrow(df) > 0 && !"DisplayAddress" %in% names(df)) {
      df$DisplayAddress <- df$Street
    }
    
    # Defend against rows from voters_raw-only fallback paths (street/name
    # searches where the matched person isn't on a route in the scenario).
    # These come in without route columns; add them so downstream code that
    # selects/uses them doesn't error.
    if (nrow(df) > 0) {
      if (!"Route"        %in% names(df)) df$Route        <- NA_character_
      if (!"RouteName"    %in% names(df)) df$RouteName    <- "Not on a route"
      if (!"StopOrder"    %in% names(df)) df$StopOrder    <- NA_integer_
      if (!"OnTargetList" %in% names(df)) df$OnTargetList <- FALSE
    }
    
    route_data(df)
    current_stop(1L)
    form_phase(1L)
  }
  
  observeEvent(input$scenario, {
    route_data(tibble::tibble())
    header_text("Voter base changed — select a route to begin.")
    current_stop(1L)
    form_phase(1L)
    
    # Update precincts for this scenario
    precincts <- voter_data() |>
      dplyr::distinct(Precinct) |>
      dplyr::arrange(Precinct) |>
      dplyr::pull(Precinct)
    updateSelectInput(session, "precinct", choices = precincts)
    
    first_precinct <- precincts[1]
    route_info <- voter_data() |>
      dplyr::filter(Precinct == first_precinct) |>
      dplyr::distinct(Route, RouteName) |>
      dplyr::arrange(Route)
    route_choices <- setNames(route_info$Route, route_info$RouteName)
    updateSelectInput(session, "route_num", choices = route_choices)
  })
  
  
  # ── Update route dropdown when precinct changes ─────────────────────
  observeEvent(input$precinct, {
    route_info <- voter_data() %>%
      dplyr::filter(Precinct == input$precinct) %>%
      dplyr::distinct(Route, RouteName) %>%
      dplyr::arrange(Route)
    route_choices <- setNames(route_info$Route, route_info$RouteName)
    updateSelectInput(session, "route_num", choices = route_choices)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$go_route, {
    df <- voter_data() %>%
      dplyr::filter(Precinct == input$precinct, Route == input$route_num) %>%
      dplyr::arrange(StopOrder)
    if (nrow(df) == 0) {
      header_text(paste0("No voters found for Precinct ", input$precinct,
                         ", Route ", input$route_num))
    } else {
      header_text(paste0("Precinct ", input$precinct, " \u2014 ",
                         df$RouteName[1], "  (", nrow(df), " voters)"))
    }
    load_route(df)
  })
  
  observeEvent(input$street_search, {
    typed <- toupper(trimws(input$street_search))
    if (nchar(typed) == 0) {
      updateSelectInput(session, "street_match", choices = c("(type above)" = ""))
      return()
    }
    
    streets <- voters_raw %>%
      dplyr::filter(stringr::str_starts(StreetNameUpper, stringr::fixed(typed))) %>%
      dplyr::distinct(StreetNameUpper, StreetSuffixUpper) %>%
      dplyr::arrange(StreetNameUpper, StreetSuffixUpper) %>%
      dplyr::mutate(label = paste(StreetNameUpper, StreetSuffixUpper))
    
    if (nrow(streets) == 0) {
      updateSelectInput(session, "street_match", choices = c("(no matches)" = ""))
    } else {
      street_choices <- setNames(
        paste(streets$StreetNameUpper, streets$StreetSuffixUpper, sep = "|"),
        streets$label
      )
      updateSelectInput(session, "street_match", choices = street_choices)
    }
  })
  
  
  observeEvent(input$go_street_all, {
    sel <- input$street_match
    if (is.null(sel) || sel == "") return()
    
    parts <- strsplit(sel, "\\|")[[1]]
    target_name <- parts[1]
    target_suffix <- parts[2]
    
    v <- voter_data()
    street_df <- v %>%
      dplyr::filter(StreetNameUpper == target_name,
                    StreetSuffixUpper == target_suffix) %>%
      dplyr::arrange(StreetNum)
    
    if (nrow(street_df) == 0) {
      # fall back to voters_raw
      street_df <- voters_raw %>%
        dplyr::filter(StreetNameUpper == target_name,
                      StreetSuffixUpper == target_suffix) %>%
        dplyr::arrange(StreetNum)
    }
    
    if (nrow(street_df) == 0) {
      header_text(paste0("No voters found on ", target_name, " ", target_suffix))
      load_route(tibble::tibble())
    } else {
      header_text(paste0(target_name, " ", target_suffix,
                         " \u2014 ", nrow(street_df), " voters"))
      load_route(street_df)
    }
  })
  
# New version to catch new voter adds, but blew up stuff.
  
    # observeEvent(input$go_street_all, {
  #   sel <- input$street_match
  #   if (is.null(sel) || sel == "") return()
  #   
  #   parts <- strsplit(sel, "\\|")[[1]]
  #   target_name   <- parts[1]
  #   target_suffix <- parts[2]
  #   
  #   # Scenario voters on this street — carry full route context.
  #   in_scen <- voter_data() %>%
  #     dplyr::filter(StreetNameUpper == target_name,
  #                   StreetSuffixUpper == target_suffix)
  #   
  #   # Everyone else on the street within the user's scope: off-scenario
  #   # voters AND newly added contacts, pulled from the full base.
  #   extra <- searchable_voters() %>%
  #     dplyr::filter(StreetNameUpper == target_name,
  #                   StreetSuffixUpper == target_suffix,
  #                   !(VoterId %in% in_scen$VoterId))
  #   
  #   street_df <- dplyr::bind_rows(in_scen, extra) %>%
  #     dplyr::arrange(StreetNum)
  #   
  #   if (nrow(street_df) == 0) {
  #     header_text(paste0("No voters found on ", target_name, " ", target_suffix))
  #     load_route(tibble::tibble())
  #   } else {
  #     header_text(paste0(target_name, " ", target_suffix,
  #                        " \u2014 ", nrow(street_df), " on street"))
  #     load_route(street_df)
  #   }
  # })
  
  observeEvent(input$go_street_route, {
    sel <- input$street_match
    if (is.null(sel) || sel == "") return()
    target_num <- as.integer(input$house_num)
    if (is.na(target_num)) {
      header_text("Enter a house number to find the route.")
      return()
    }
    
    parts <- strsplit(sel, "\\|")[[1]]
    target_name <- parts[1]
    target_suffix <- parts[2]
    
    street_df <- voters_raw %>%
      dplyr::filter(StreetNameUpper == target_name,
                    StreetSuffixUpper == target_suffix)
    
    if (nrow(street_df) == 0) {
      header_text(paste0("No records found on ", target_name, " ", target_suffix))
      load_route(tibble::tibble())
      return()
    }
    
    exact <- street_df %>% dplyr::filter(StreetNum == target_num)
    if (nrow(exact) > 0) {
      matched_vid <- exact$VoterId[1]
      label <- paste0(target_num, " ", target_name, " ", target_suffix, " \u2014 exact match")
    } else {
      lower <- street_df %>% dplyr::filter(StreetNum < target_num)
      if (nrow(lower) > 0) {
        lower <- lower %>% dplyr::filter(StreetNum == max(StreetNum))
        matched_vid <- lower$VoterId[1]
        label <- paste0(target_num, " not found; nearest lower ", lower$StreetNum[1])
      } else {
        higher <- street_df %>% dplyr::filter(StreetNum > target_num)
        if (nrow(higher) > 0) {
          higher <- higher %>% dplyr::filter(StreetNum == min(StreetNum))
          matched_vid <- higher$VoterId[1]
          label <- paste0(target_num, " not found; nearest higher ", higher$StreetNum[1])
        } else {
          header_text(paste0("No nearby houses found on ", target_name, " ", target_suffix))
          load_route(tibble::tibble())
          return()
        }
      }
    }
    
    v <- voter_data()
    person <- v %>% dplyr::filter(VoterId == matched_vid)
    
    if (nrow(person) == 0) {
      person <- voters_raw %>% dplyr::filter(VoterId == matched_vid)
      header_text(paste0(label, " (not in current voter base)"))
      load_route(person)
      return()
    }
    
    matched_route <- person$Route[1]
    matched_precinct <- person$Precinct[1]
    
    if (is.na(matched_route)) {
      header_text(paste0(label, " (not on a route)"))
      load_route(person)
    } else {
      df <- v %>%
        dplyr::filter(Precinct == matched_precinct, Route == matched_route) %>%
        dplyr::arrange(StopOrder)
      header_text(paste0(label, " \u2192 ", person$RouteName[1],
                         "  (", nrow(df), " voters)"))
      load_route(df)
    }
  })

  
  # ── Dynamic first-name dropdown based on last name ─────────────────────
  
  observeEvent(input$search_last, {
    last <- toupper(trimws(input$search_last))
    #last <- stringr::fixed(last)
    if (nchar(last) == 0) {
      updateSelectizeInput(session, "search_first",
                        choices = c("(type last name first)" = ""),
                        server=TRUE)
      return()
    }
    # Search within the user's role scope (all scenarios their role can
    # access), not the currently-selected scenario. This lets an NS user
    # look up a Republican by name while canvassing NSDem — but stops a
    # Pawtucket user from searching a D17 voter and vice versa.
    v <- searchable_voters()
    matches <- v %>%
      dplyr::filter(stringr::str_starts(LastNameUpper, stringr::fixed(last)) |
                      stringr::str_detect(LastNameUpper, stringr::fixed(paste0(" ", last)))) %>%
      dplyr::arrange(LastName, FirstName) %>%
      dplyr::mutate(label = paste0(FirstName, " ", LastName))
    
    dup <- matches$label[duplicated(matches$label) |
                           duplicated(matches$label, fromLast = TRUE)]
    matches <- matches %>%
      dplyr::mutate(label = ifelse(label %in% dup,
                                   paste0(label, " \u2014 ", StreetNum, " ", StreetName),
                                   label))
    if (nrow(matches) == 0) {
      updateSelectizeInput(session, "search_first",
                        choices = c("(no matches)" = ""),
                        server=TRUE)
    } else {
      # Show "First Last" as labels, use row index to look up later
      name_choices <- setNames(matches$RowID, matches$label)
      updateSelectizeInput(session, "search_first", choices = name_choices,
                           server=TRUE)
    }
  })
  
  # ── Name search → load that voter's route ──────────────────────────────
  
  observeEvent(input$go_name, {
    selected_id <- as.integer(input$search_first)
    if (is.na(selected_id)) return()
    
    # Look up the selected person within the user's role scope, matching
    # what the dropdown offered.
    person <- searchable_voters() %>% dplyr::filter(RowID == selected_id)
    
    if (nrow(person) == 0) {
      header_text("No voter found with that selection.")
      load_route(tibble::tibble())
      return()
    }
    
    matched_precinct <- person$Precinct[1]
    # person is from voters_raw so doesn't have Route/RouteName — look
    # those up in voter_data() (the scenario-joined view) by VoterId.
    v <- voter_data()
    route_row <- v %>% dplyr::filter(VoterId == person$VoterId[1])
    matched_route <- if (nrow(route_row) > 0) route_row$Route[1] else NA
    
    if (is.na(matched_route)) {
      # Voter isn't on a route in the current scenario — load just them.
      # Fall back to what voter_data() would have shown for this person
      # if they're off-target, or the voters_raw row if not in scenario at all.
      if (nrow(route_row) > 0) {
        df <- route_row
      } else {
        df <- person |>
          dplyr::mutate(
            Route        = NA_character_,
            RouteName    = "Not on a route",
            StopOrder    = NA_integer_,
            Scenario     = input$scenario,
            OnTargetList = FALSE
          )
      }
      
      header_text(paste0(person$FirstName[1], " ", person$LastName[1],
                         " \u2014 Precinct ", matched_precinct,
                         " (not on a route in this scenario)"))
    } else {
      df <- v %>%
        dplyr::filter(Precinct == matched_precinct, Route == matched_route) %>%
        dplyr::arrange(StopOrder)
      
      header_text(paste0(person$FirstName[1], " ", person$LastName[1],
                         " \u2014 Precinct ", matched_precinct,
                         ", ", route_row$RouteName[1],
                         "  (", nrow(df), " voters)"))
    }
    
    if (nrow(df) > 0 && !"DisplayAddress" %in% names(df)) {
      df$DisplayAddress <- df$Street
    }
    
    route_data(df)
    match_row <- df[df$RowID == selected_id, , drop = FALSE]
    if (nrow(match_row) > 0) {
      so  <- sort(unique(df$StopOrder))
      pos <- which(so == match_row$StopOrder[1])
      current_stop(if (length(pos) > 0) pos[1] else 1L)
    } else {
      current_stop(1L)
    }
    
    #open contact_card tab if not already open
    bslib::nav_select(id = "main_tabs", selected = "contact_card", session = session)
    
    form_phase(1L)
  })
  
  # ── Stop navigation ───────────────────────────────────────────────────
  
  observeEvent(input$prev_stop, {
    n <- length(stop_orders())
    if (n < 1) return()
    cur <- current_stop()
    current_stop(if (cur > 1) cur - 1L else n)    # at first → wrap to last
    form_phase(1L)
  })
  
  observeEvent(input$next_stop, {
    n <- length(stop_orders())
    if (n < 1) return()
    cur <- current_stop()
    current_stop(if (cur < n) cur + 1L else 1L)   # at last → wrap to first
    form_phase(1L)
  })
  
  observeEvent(input$they_moved, {
    v <- current_voter()
    if (is.null(v)) return()
    vid <- as.character(v$VoterId)
    
    # DB flag
    flag_may_have_moved(con, vid)
    
    # In-memory voters_raw — so the voter base table / future searches see it
    vr <- get("voters_raw", envir = globalenv())
    vr$MayHaveMoved[vr$VoterId == vid] <- 1L
    assign("voters_raw", vr, envir = globalenv())
    
    # The loaded route — so the 📦 badge shows on THIS card immediately
    rd <- route_data()
    if (nrow(rd) > 0) {
      rd$MayHaveMoved[rd$VoterId == vid] <- 1L
      route_data(rd)
    }
    
    data_refresh(data_refresh() + 1)
    showNotification("Marked as may have moved.", type = "message")
  })
  
  observeEvent(input$go_stop, {
    n <- length(stop_orders())
    if (n < 1) return()
    target <- suppressWarnings(as.integer(input$jump_stop))
    if (is.na(target)) return()
    current_stop(max(1L, min(n, target)))   # clamp into [1, n]
    form_phase(1L)
  })
  
  # ── Current household / voter reactives ───────────────────────────────
  # current_stop() is now a POSITION into the unique, sorted StopOrder
  # values for this route — not a row index into route_data(). A house
  # with 2 registered voters shares one StopOrder, so it counts as ONE
  # stop instead of two.
  
  stop_orders <- reactive({
    rd <- route_data()
    if (nrow(rd) == 0) return(integer(0))
    sort(unique(rd$StopOrder))
  })
  
  current_household <- reactive({
    rd <- route_data()
    so <- stop_orders()
    idx <- current_stop()
    if (nrow(rd) == 0 || length(so) == 0 || idx < 1 || idx > length(so)) {
      return(rd[0, ])
    }
    rd[rd$StopOrder == so[idx], , drop = FALSE]
  })
  
  # Back-compat: anything still expecting "a single voter at this stop"
  # (header text, they_moved, modals) reads the FIRST row of the household.
  # This keeps every downstream single-voter house behaving exactly as
  # before — only multi-voter houses change.
  current_voter <- reactive({
    hh <- current_household()
    if (nrow(hh) == 0) return(NULL)
    hh[1, ]
  })
  
  # ── Contact header ────────────────────────────────────────────────────
  
  output$contact_header <- renderText({
    rd <- route_data()
    if (nrow(rd) == 0) return("Search for or get a route.")
    paste0("Precinct ", rd$Precinct[1], " \u2014 ", rd$RouteName[1],
           "  |  Stop ", current_stop(), " of ", length(stop_orders()))
  })
  
  # ── Voter info panel ──────────────────────────────────────────────────
  
  output$voter_info_panel <- renderUI({
    hh <- current_household()
    if (nrow(hh) == 0) return(tags$p("No route loaded."))
    
    data_refresh()  # re-evaluate when a save bumps this
    
    # House-level facts — taken from the first row since these describe
    # the address, not a specific person.
    v1 <- hh[1, ]
    
    # ── Per-person line: name + icons + age/party/VS + last-contact ──────
    build_person_line <- function(v) {
      vid <- as.character(v$VoterId)
      is_off_target <- isTRUE(!v$OnTargetList)
      line_style <- if (is_off_target) {
        "padding: 0.4rem; border-radius: 0.4rem; background-color: #FFF8E1; border-left: 3px solid #D4A017;"
      } else {
        "padding: 0.4rem 0;"
      }
      off_target_note <- if (is_off_target) {
        tags$span(style = "font-size: 0.8em; color: #8B6F00;", tags$em("(Off Route)"))
      } else NULL
      name_elements <- list(tags$strong(v$Name))
      
      who <- if (!is.null(v$WhoKnows) && !is.na(v$WhoKnows) && v$WhoKnows != "") v$WhoKnows else ""
      if (who != "") {
        name_elements <- c(name_elements, list(
          tags$span(class = "badge ms-2",
                    style = "background-color: #6E120B; color: #FFFFFF;", who)
        ))
      }
      
      donate <- if (!is.null(v$DonateText) && !is.na(v$DonateText) && v$DonateText != "") v$DonateText else ""
      if (donate != "") {
        name_elements <- c(name_elements, list(
          tags$span(class = "ms-2", style = "color: #6E120B;", "\U0001F4B2")
        ))
      }
      
      Gender <- if (!is.null(v$Gender) && !is.na(v$Gender) && v$Gender != "") v$Gender else ""
      if (Gender %in% c("M", "F")) {
        icon <- if (Gender == "M") bsicons::bs_icon("gender-male") else bsicons::bs_icon("gender-female")
        name_elements <- c(name_elements, list(tags$span(class = "ms-2", style = "color: #6E120B;", icon)))
      }
      
      bond <- if (!is.null(v$nsse_2026_voter) && !is.na(v$nsse_2026_voter) &&
                  v$nsse_2026_voter == "Yes") TRUE else FALSE
      if (bond) {
        name_elements <- c(name_elements, list(
          tags$span(class = "ms-2", title = "NSSE 2026 Voter", "\U0001F694")
        ))
      }
      
      spoken <- if (!is.null(v$TimesSpoken) && !is.na(v$TimesSpoken) && v$TimesSpoken > 0) v$TimesSpoken else 0
      if (spoken > 0) {
        name_elements <- c(name_elements, list(tags$span(class = "ms-2", "\U0001F3A4")))
      }
      
      inactive <- if (!is.null(v$VoterStatus) && !is.na(v$VoterStatus) && v$VoterStatus == "Inactive") TRUE else FALSE
      if (inactive) {
        name_elements <- c(name_elements, list(
          tags$span(class = "ms-2", style = "color: #6E120B;", bsicons::bs_icon("exclamation-triangle-fill"))
        ))
      }
      
      may_moved <- if (!is.null(v$MayHaveMoved) && !is.na(v$MayHaveMoved) && v$MayHaveMoved == 1) TRUE else FALSE
      if (may_moved) {
        name_elements <- c(name_elements, list(
          tags$span(class = "ms-2", title = "May have moved — a new contact was recorded here", "\U0001F4E6")
        ))
      }
      
      material_left <- voter_left_material_since(con, vid, "2026-07-07")
      if (material_left) {
        name_elements <- c(name_elements, list(
          tags$span(class = "ms-2", style = "color: #6E120B;",
                    title = "Left material on or after July 7th",
                    bsicons::bs_icon("file-earmark-check"))
        ))
      }
      
      party_display <- dplyr::case_when(
        v$Party == "U" ~ "Ind",
        v$Party == "R" ~ "Rep",
        v$Party == "D" ~ "Dem",
        is.na(v$Party) ~ "Unk",
        TRUE ~ v$Party
      )
      
      latest <- latest_contact(con, vid)
      prev_line <- NULL
      if (nrow(latest) > 0) {
        lv <- function(field, fallback = NA_character_) {
          val <- latest[[field]]
          if (is.null(val) || length(val) == 0 || is.na(val) || !nzchar(val)) fallback else as.character(val)
        }
        saved_support <- lv("support_rating", "")
        saved_c1 <- lv("concern1", "(none)"); saved_c1o <- lv("concern1_other", "")
        saved_c2 <- lv("concern2", "(none)"); saved_c2o <- lv("concern2_other", "")
        saved_c3 <- lv("concern3", "(none)"); saved_c3o <- lv("concern3_other", "")
        
        resolve_concern <- function(concern, other) {
          if (is.na(concern) || concern == "" || concern == "(none)") return(NA_character_)
          if (concern == "Other" && !is.na(other) && nzchar(other)) return(other)
          concern
        }
        concerns <- c(resolve_concern(saved_c1, saved_c1o),
                      resolve_concern(saved_c2, saved_c2o),
                      resolve_concern(saved_c3, saved_c3o))
        concerns_display <- paste(concerns[!is.na(concerns)], collapse = ", ")
        
        prev_line <- tags$span(
          style = "font-size: 0.85em; color: #666;",
          if (saved_support != "") tagList(tags$em("Last support: "), saved_support, "") else NULL,
          if (saved_support != "" && nzchar(concerns_display)) tags$br() else NULL,
          if (nzchar(concerns_display)) tagList(tags$em("Concerns: "), concerns_display) else NULL
        )
      }
      
      pipeline_addl  <- if (!is.null(v$Additional) && !is.na(v$Additional) && v$Additional != "") v$Additional else ""
      canvasser_addl <- get_additional_notes(con, vid)
      additional <- paste(c(pipeline_addl, canvasser_addl)[nzchar(c(pipeline_addl, canvasser_addl))], collapse = " | ")
      
      tags$div(
        style = paste0(line_style, " border-bottom: 1px solid #E0E0E0;"),
        tags$p(class = "mb-1", name_elements, if (!is.null(off_target_note)) tagList(" ", off_target_note) else NULL),
        tags$p(class = "mb-1", style = "font-size: 0.9em;",
               paste0(v$Age, " y.o.", " | ", party_display, " | ", "VS: ", v$VoterScore)),
        if (!is.null(prev_line)) tags$p(class = "mb-1", prev_line) else NULL,
        if (nzchar(additional)) tags$p(class = "mb-0", style = "font-size: 0.85em; color: #444;", additional) else NULL
      )
    }
    
    person_lines <- lapply(seq_len(nrow(hh)), function(i) build_person_line(hh[i, ]))
    
    tagList(
      tags$div(
        tags$p(v1$DisplayAddress %||% v1$Street),
        tags$div(person_lines)
      )
    )
  })
  
  registerVoterContactModals(
    input, output, session,
    current_voter   = current_voter,
    route_data      = route_data,
    voter_data      = voter_data,
    data_refresh    = data_refresh,
    credentials     = credentials,
    con             = con,
    who_initials    = who_initials,
    concern_choices = concern_choices,
    user_scenarios  = user_scenarios,
    user_role = user_role
  )
  
  password_change_server("pw_mod",
                       credentials  = credentials,
                       con          = con
  )
  
  # Mirrors sanitize() in split_canvass_pdf.py so filenames match exactly
  sanitize_route_name <- function(name) {
    trimws(gsub('[\\\\/:*?"<>|]', "-", name))
  }
  
  output$print_route_link <- renderUI({
    req(input$scenario, input$precinct, input$route_num)
    
    rn <- routes_raw %>%
      filter(Scenario == input$scenario,
             as.character(Precinct) == input$precinct,
             as.character(Route) == input$route_num) %>%
      pull(RouteName) %>%
      unique()
    
    if (length(rn) == 0) return(NULL)
    
    fname <- paste0(input$scenario, "_P", input$precinct, "_",
                    sanitize_route_name(rn[1]), ".pdf")
    fpath <- file.path("www", "canvass_pdfs", fname)
    
    if (file.exists(fpath)) {
      tags$a(href = file.path("canvass_pdfs", utils::URLencode(fname)), target = "_blank",
             class = "btn btn-outline-secondary btn-sm w-100 mt-1",
             bsicons::bs_icon("printer"), " Print this route")
    } else {
      tags$small(class = "text-muted", "No printable sheet for this route yet.")
    }
  })
  
  output$canvass_form <- renderUI({
    v <- current_voter()
    if (is.null(v)) {
      return(tagList(
        tags$p(class = "text-muted text-center",
               "Pick routes from sidebar, or let KEVIN suggest one."),
        tags$div(class = "text-center mt-3",
                 actionButton("suggest_route", 
                              label = tagList(bsicons::bs_icon("compass"), 
                                              " Get me a route"),
                              class = "btn-primary btn-lg")
        ),
        uiOutput("suggest_route_status")
      ))
    }
    
    phase <- form_phase()
    
    # ── Look up this voter's latest contact row from the DB ─────────
    vid <- as.character(v$VoterId)
    data_refresh()  # re-render when saves happen
    latest <- latest_contact(con, vid)
    has_contact <- nrow(latest) > 0
    
    # Helper: read a field from the latest contact, with fallback
    lv <- function(field, fallback = NA_character_) {
      if (!has_contact) return(fallback)
      val <- latest[[field]]
      if (is.null(val) || length(val) == 0 || is.na(val) || !nzchar(val)) fallback else as.character(val)
    }
    
    saved_campaigner <- lv("campaigner",       user_campaigner(credentials()$info$user))
    saved_left_mat   <- lv("left_material",    "N")
    saved_were_there <- lv("were_they_there",  "N")
    saved_had_convo  <- lv("had_conversation", "N")
    saved_support    <- lv("support_rating",   character(0))
    saved_c1         <- lv("concern1",         "(none)")
    saved_c2         <- lv("concern2",         "(none)")
    saved_c3         <- lv("concern3",         "(none)")
    saved_c1o        <- lv("concern1_other",   "")
    saved_c2o        <- lv("concern2_other",   "")
    saved_c3o        <- lv("concern3_other",   "")
    
    # ── PHASE 1: base questions ──────────────────────────────────────────
    if (phase == 1L) {
      role  <- user_role()
      is_ns <- role %in% c("ns", "admin")
      is_sb <- role == "sb"
      u     <- tolower(credentials()$info$user)
      hide_campaigner <- u %in% c("kcarriere", "pjones")
      method <- if (is.null(input$contact_method)) "knock" else input$contact_method
      
      campaigner_input <-
        if (is_ns && !hide_campaigner)
          radioButtons("campaigner", "Knocking for who?",
                       choices = c("Paul", "Kevin"),
                       selected = saved_campaigner, inline = TRUE)
      else if (is_sb && !hide_campaigner)
        radioButtons("campaigner", "Knocking for who?",
                     choices = c("Gabbie", "Mark"),
                     selected = saved_campaigner, inline = TRUE)
      else NULL
      
      method_toggle <- tags$div(
        style = "font-size: 0.85em;",
        radioButtons("contact_method", NULL,
                     choices  = c("\u270A\U0001F3FB Knock" = "knock",
                                  "\U0001F4E7 Email"        = "email"),
                     selected = method, inline = TRUE)
      )
      
      # Email path: the "not-home / sent an email instead" short path.
      # No door questions, no conversation, no support. were_they_there
      # and left_material stay NULL so emails don't count as knocked doors.
      if (identical(method, "email")) {
        return(tagList(
          method_toggle,
          campaigner_input,
          tags$hr(),
          tags$p(class = "text-muted", "Logs an email to this voter."),
          layout_columns(
            col_widths = c(6, 6),
            actionButton("save_contact",  "\U0001F4BE Save",              class = "btn-success w-100"),
            actionButton("save_and_next", "\U0001F4BE Save & Next \u25B6", class = "btn-primary w-100")
          ),
          uiOutput("save_status")
        ))
      }
      
      # Knock path (default) — Were-there first, then Left material.
      return(tagList(
        method_toggle,
        campaigner_input,
        radioButtons("were_there", "Was anyone home?",
                     choices = c("Yes", "No"),
                     selected = saved_were_there, inline = TRUE),
        radioButtons("left_material", "Did you leave campaign material?",
                     choices = c("Yes", "No"),
                     selected = saved_left_mat, inline = TRUE),
        tags$hr(),
        actionButton("phase1_next", "Continue \u25B6", class = "btn-primary w-100")
      ))
    }
    
    # ── PHASE 2: they were there → conversation details ──────────────────
    if (phase == 2L) {
      return(tagList(
        tags$p(class = "text-muted",
               paste0("\u2713 ",
                      campaigner_resolved(),
                      " | Material: ", input$left_material,
                      " | There: ", input$were_there)),
        tags$hr(),
        radioButtons("had_conversation", "Had a conversation?",
                     choices = c("Yes", "No"),
                     selected = saved_had_convo, inline = TRUE),
        tags$hr(),
        layout_columns(
          col_widths = c(4, 8),
          actionButton("phase2_back", "\u25C0 Back", class = "btn-outline-secondary w-100"),
          actionButton("phase2_next", "Continue \u25B6", class = "btn-primary w-100")
        )
      ))
    }
    
    # ── PHASE 3: had conversation → support + concerns ───────────────────
    if (phase == 3L) {
      hh  <- current_household()
      tid <- talked_to_ids()
      
      # ── Who did you talk to? (only when >1 person and not yet chosen) ──
      if (nrow(hh) > 1 && is.null(tid)) {
        choices <- setNames(as.character(hh$VoterId), hh$Name)
        return(tagList(
          tags$p(class = "text-muted",
                 paste0("\u2713 ", campaigner_resolved(),
                        " | Material: ", input$left_material,
                        " | Anyone home: Y | Conversation: Y")),
          tags$hr(),
          checkboxGroupInput("talked_to_select", "Who did you talk to?", choices = choices),
          tags$hr(),
          layout_columns(
            col_widths = c(4, 8),
            actionButton("phase3_back", "\u25C0 Back", class = "btn-outline-secondary w-100"),
            actionButton("confirm_talked_to", "Continue \u25B6", class = "btn-primary w-100")
          )
        ))
      }
      
      # ── Per-person support/concerns card ────────────────────────────────
      qi <- person_queue_idx()
      if (is.null(tid) || qi < 1 || qi > length(tid)) {
        return(tags$p("Nothing to show \u2014 go back and try again."))
      }
      
      cur_vid    <- tid[qi]
      cur_person <- hh[as.character(hh$VoterId) == cur_vid, , drop = FALSE][1, ]
      
      stashed         <- person_answers()[[cur_vid]]
      cur_latest      <- latest_contact(con, cur_vid)
      has_cur_contact <- nrow(cur_latest) > 0
      lvp <- function(field, fallback = NA_character_) {
        if (!has_cur_contact) return(fallback)
        val <- cur_latest[[field]]
        if (is.null(val) || length(val) == 0 || is.na(val) || !nzchar(val)) fallback else as.character(val)
      }
      
      p_support <- stashed$support_rating %||% lvp("support_rating", character(0))
      p_c1      <- stashed$concern1       %||% lvp("concern1", "(none)")
      p_c1o     <- stashed$concern1_other %||% lvp("concern1_other", "")
      p_c2      <- stashed$concern2       %||% lvp("concern2", "(none)")
      p_c2o     <- stashed$concern2_other %||% lvp("concern2_other", "")
      p_c3      <- stashed$concern3       %||% lvp("concern3", "(none)")
      p_c3o     <- stashed$concern3_other %||% lvp("concern3_other", "")
      p_addl    <- stashed$contact_addl %||% ""
      
      progress_note <- if (length(tid) > 1) paste0(" (", qi, " of ", length(tid), ")") else ""
      
      return(tagList(
        tags$p(class = "text-muted", paste0("\u2713 Talking to: ", cur_person$Name, progress_note)),
        tags$hr(),
        selectInput("support_rating", paste0("How supportive was ", cur_person$Name, "?"),
                    choices = c(
                      "Select..." = "",
                      "1 \u2013 You have my vote" = "1",
                      "2 \u2013 Lean for" = "2",
                      "3 \u2013 No opinion" = "3",
                      "4 \u2013 Lean against" = "4",
                      "5 \u2013 Strongly against" = "5"),
                    selected = p_support),
        tags$hr(),
        selectInput("concern1", paste0(cur_person$Name, "'s #1 Concern"), choices = concern_choices, selected = p_c1, width = "100%"),
        uiOutput("concern1_other_ui"),
        selectInput("concern2", paste0(cur_person$Name, "'s #2 Concern"), choices = concern_choices, selected = p_c2, width = "100%"),
        uiOutput("concern2_other_ui"),
        selectInput("concern3", paste0(cur_person$Name, "'s #3 Concern"), choices = concern_choices, selected = p_c3, width = "100%"),
        uiOutput("concern3_other_ui"),
        tags$hr(),
        textInput("contact_addl", "Additional information?",
                  value = p_addl, width = "100%",
                  placeholder = "for context on choices; or other."),
        tags$hr(),
        layout_columns(
          col_widths = c(4, 8),
          actionButton("phase3_back", "\u25C0 Back", class = "btn-outline-secondary w-100"),
          actionButton("person_next", "Continue \u25B6", class = "btn-primary w-100")
        )
      ))
    }
    
    # ── PHASE 4: they weren't there / no conversation → just save ────────
    if (phase == 4L) {
      hh  <- current_household()
      tid <- talked_to_ids()
      
      campaign_text <- if (!is.null(campaigner_resolved())) paste0(campaigner_resolved(), " | ") else ""
      summary_text <- paste0("\u2713 ", campaign_text,
                             "Material: ", input$left_material,
                             " | Anyone home: ", input$were_there)
      if (!is.null(input$were_there) && input$were_there == "Yes") {
        summary_text <- paste0(summary_text, " | Conversation: ",
                               if (!is.null(tid) && length(tid) > 0) "Yes" else "No")
      }
      
      roster_lines <- if (!is.null(tid) && length(tid) > 0) {
        tagList(
          tags$p(tags$strong("Talked to: "),
                 paste(hh$Name[as.character(hh$VoterId) %in% tid], collapse = ", ")),
          if (nrow(hh) > length(tid)) {
            tags$p(tags$strong("Home, no direct conversation: "),
                   paste(hh$Name[!as.character(hh$VoterId) %in% tid], collapse = ", "))
          }
        )
      } else NULL
      
      return(tagList(
        tags$p(class = "text-muted", summary_text),
        roster_lines,
        tags$hr(),
        tags$p("Ready to save this stop \u2014 this records a contact for everyone at this address."),
        layout_columns(
          col_widths = c(4, 4, 4),
          actionButton("phase4_back", "\u25C0 Back", class = "btn-outline-secondary w-100"),
          actionButton("save_contact", "\U0001F4BE Save Contact", class = "btn-success w-100"),
          actionButton("save_and_next", "\U0001F4BE Save & Next \u25B6", class = "btn-primary w-100")
        ),
        uiOutput("save_status")
      ))
    }
  })
  
  # ── Phase transitions ─────────────────────────────────────────────────
  
  observeEvent(input$phase1_next, {
    if (!is.null(input$were_there) && input$were_there == "Yes") {
      form_phase(2L)
    } else {
      form_phase(4L)
    }
  })
  
  observeEvent(input$phase2_back, { form_phase(1L) })
  
  observeEvent(input$phase2_next, {
    if (!is.null(input$had_conversation) && input$had_conversation == "Yes") {
      hh <- current_household()
      if (nrow(hh) <= 1) {
        talked_to_ids(as.character(hh$VoterId))
      } else {
        talked_to_ids(NULL)
      }
      person_queue_idx(1L)
      person_answers(list())
      form_phase(3L)
    } else {
      form_phase(4L)
    }
  })
  
  observeEvent(input$confirm_talked_to, {
    sel <- input$talked_to_select
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Select at least one person.", type = "error")
      return()
    }
    talked_to_ids(sel)
    person_queue_idx(1L)
  })
  
  observeEvent(input$person_next, {
    tid <- talked_to_ids()
    if (is.null(tid)) return()
    qi <- person_queue_idx()
    if (qi < 1 || qi > length(tid)) return()
    cur_vid <- tid[qi]
    
    ans <- person_answers()
    ans[[cur_vid]] <- list(
      support_rating = input$support_rating,
      concern1       = input$concern1,
      concern1_other = input$concern1_other,
      concern2       = input$concern2,
      concern2_other = input$concern2_other,
      concern3       = input$concern3,
      concern3_other = input$concern3_other,
      contact_addl   = input$contact_addl
    )
    person_answers(ans)
    
    if (qi < length(tid)) {
      person_queue_idx(qi + 1L)
    } else {
      form_phase(4L)
    }
  })
  
  observeEvent(input$phase3_back, {
    tid <- talked_to_ids()
    hh  <- current_household()
    qi  <- person_queue_idx()
    
    if (!is.null(tid) && qi > 1) {
      person_queue_idx(qi - 1L)          # previous person's card
    } else if (!is.null(tid) && nrow(hh) > 1) {
      talked_to_ids(NULL)                # back to the picker
    } else {
      form_phase(2L)                     # out of phase 3 entirely
    }
  })
  
  
  observeEvent(input$phase4_back, {
    tid <- talked_to_ids()
    if (!is.null(tid) && length(tid) > 0 &&
        !is.null(input$had_conversation) && input$had_conversation == "Yes") {
      person_queue_idx(length(tid))      # re-enter loop at the last person
      form_phase(3L)
    } else if (!is.null(input$were_there) && input$were_there == "Yes") {
      form_phase(2L)
    } else {
      form_phase(1L)
    }
  })
  
  # ── Re-render concern "Other" fields when dropdown changes ────────────
  # Since we're fully server-rendered, we need to re-render phase 3
  # when a concern dropdown changes to/from "Other".
  # We do this by watching the concern inputs and bumping the phase to re-render.
  output$concern1_other_ui <- renderUI({
    req(input$concern1)
    if (input$concern1 == "Other") {
      textInput("concern1_other", NULL, placeholder = "Specify...")
    }
  })
  
  output$concern2_other_ui <- renderUI({
    req(input$concern2)
    if (input$concern2 == "Other") {
      textInput("concern2_other", NULL, placeholder = "Specify...")
    }
  })
  
  output$concern3_other_ui <- renderUI({
    req(input$concern3)
    if (input$concern3 == "Other") {
      textInput("concern3_other", NULL, placeholder = "Specify...")
    }
  })
  
  # ── Save logic ────────────────────────────────────────────────────────
  
  campaigner_resolved <- reactive({
    u <- tolower(credentials()$info$user)
    if (u %in% c("kcarriere", "pjones")) return(user_campaigner(u))
    if (!is.null(input$campaigner)) input$campaigner else user_campaigner(u)
  })
  
  do_save <- function() {
    hh <- current_household()
    if (nrow(hh) == 0) return(FALSE)
    
    method <- if (is.null(input$contact_method)) "knock" else input$contact_method
    
    # Email path unchanged — a single logged email against current_voter().
    if (identical(method, "email")) {
      v <- current_voter()
      if (is.null(v)) return(FALSE)
      insert_contact(con, as.character(v$VoterId),
                     input_by = credentials()$info$user,
                     fields = list(
                       campaigner   = validate_choice(campaigner_resolved(), validators$campaigner_choice),
                       contact_type = "email"
                     ))
      data_refresh(data_refresh() + 1)
      return(TRUE)
    }
    
    were_there_val     <- validate_choice(input$were_there, validators$were_they_there_choice)
    left_material_val  <- validate_choice(input$left_material, validators$left_material_choice)
    campaigner_val      <- validate_choice(campaigner_resolved(), validators$campaigner_choice)
    
    tid     <- talked_to_ids()
    answers <- person_answers()
    
    for (i in seq_len(nrow(hh))) {
      v      <- hh[i, ]
      vid    <- as.character(v$VoterId)
      talked <- !is.null(tid) && vid %in% tid
      
      if (talked) {
        ans <- answers[[vid]] %||% list()
        had_convo_val       <- "Yes"
        support_rating_val  <- validate_choice(ans$support_rating, validators$support_rating_choice)
        concern1_val        <- validate_choice(ans$concern1, validators$concern1_choice)
        concern2_val        <- validate_choice(ans$concern2, validators$concern2_choice)
        concern3_val        <- validate_choice(ans$concern3, validators$concern3_choice)
        concern1_other_val  <- if (identical(ans$concern1, "Other") && !is.null(ans$concern1_other)) sanitize_user_input(ans$concern1_other) else NA_character_
        concern2_other_val  <- if (identical(ans$concern2, "Other") && !is.null(ans$concern2_other)) sanitize_user_input(ans$concern2_other) else NA_character_
        concern3_other_val  <- if (identical(ans$concern3, "Other") && !is.null(ans$concern3_other)) sanitize_user_input(ans$concern3_other) else NA_character_
        addl_text           <- if (!is.null(ans$contact_addl)) trimws(ans$contact_addl) else ""
      } else {
        had_convo_val <- if (!is.null(input$were_there) && input$were_there == "Yes") "No" else NA_character_
        support_rating_val <- NA_character_
        concern1_val <- NA_character_; concern1_other_val <- NA_character_
        concern2_val <- NA_character_; concern2_other_val <- NA_character_
        concern3_val <- NA_character_; concern3_other_val <- NA_character_
        addl_text <- ""
      }
      
      fields <- list(
        campaigner       = campaigner_val,
        contact_type     = "knock",
        left_material    = left_material_val,
        were_they_there  = were_there_val,
        had_conversation = had_convo_val,
        support_rating   = support_rating_val,
        concern1         = concern1_val,
        concern2         = concern2_val,
        concern3         = concern3_val,
        concern1_other   = concern1_other_val,
        concern2_other   = concern2_other_val,
        concern3_other   = concern3_other_val
      )
      
      insert_contact(con, vid, input_by = credentials()$info$user, fields = fields)
      
      if (talked && nzchar(addl_text)) {
        insert_additional_info(
          con,
          vid             = vid,
          who_knows       = NA_character_,
          additional_text = sanitize_user_input(addl_text),
          updated_by      = credentials()$info$user
        )
      }
    }
    
    data_refresh(data_refresh() + 1)
    TRUE
  }
  
  observeEvent(input$save_contact, {
    if (do_save()) {
      output$save_status <- renderUI(
        tags$div(class = "alert alert-success mt-2 py-1",
                 paste0("\u2713 Saved stop ", current_stop(), " at ", format(Sys.time(), "%H:%M")))
      )
    }
  })
  
  observeEvent(input$save_and_next, {
    if (do_save()) {
      if (current_stop() < length(stop_orders())) {
        current_stop(current_stop() + 1L)
        form_phase(1L)
      }
      output$save_status <- renderUI(
        tags$div(class = "alert alert-success mt-2 py-1",
                 paste0("\u2713 Saved & advanced to stop ", current_stop()))
      )
    }
  })
  
  # ── Voter Base Table ────────────────────────────────────────────────────────
  voterListVoterBaseServer("voter_base_mod",
                           con               = con,
                           searchable_voters = searchable_voters,
                           credentials       = credentials,
                           data_refresh      = data_refresh,
                           who_initials      = who_initials
  )
  
  
  output$voter_list_header <- renderText({
    if (input$voter_list_mode == "voterbase") {
      return("All Voters View")
    }
    # Route mode — use existing header_text() logic
    header_text()
  })
  
  output$voter_list_results <- renderUI({
    if (input$voter_list_mode == "route") {
      DTOutput("voter_list_route_table")
    } else {
      voterListVoterBaseTableUI("voter_base_mod")
    }
  })
  
  # ── Route mode table ──
  output$voter_list_route_table <- DT::renderDT({
    rd <- route_data()
    if (nrow(rd) == 0) {
      return(DT::datatable(
        tibble::tibble(Notice = "Use the sidebar to search by route or street."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    data_refresh()
    contacted_ids <- voters_with_any_contact(con)
    
    display <- rd %>%
      dplyr::mutate(
        Recorded = ifelse(as.character(VoterId) %in% contacted_ids, "\u2713", "")
      ) %>%
      dplyr::select(StopOrder, Name, Street = DisplayAddress, Age, Party, VoterScore, Recorded)
    
    DT::datatable(display,
                  options = list(pageLength = 50, dom = "tip"),
                  rownames = FALSE)
  })
  
  # ── Direction links ────────────────────────────────────────────────────
  
  output$direction_links <- renderUI({
    rd <- route_data()
    if (nrow(rd) == 0) return(tags$p("No route loaded yet."))
    addresses <- rd %>% dplyr::arrange(StopOrder) %>% dplyr::distinct(Street) %>% dplyr::pull(Street)
    if (length(addresses) < 2) return(tags$p("Need at least 2 stops."))
    max_stops <- if (input$use_phone) 5L else 11L
    links <- build_direction_links(addresses, max_stops)
    if (length(links) == 0) return(tags$p("Could not build links."))
    link_tags <- lapply(links, function(lnk) {
      tags$p(tags$a(href = lnk$url, target = "_blank", lnk$label))
    })
    tagList(
      tags$p(tags$strong(paste0(length(addresses), " stops \u2192 ",
                                length(links), " link(s)",
                                if (input$use_phone) " [mobile]" else " [desktop]"))),
      link_tags
    )
  })
  
  # ── Progress Bar ────────────────────────────────────────────────────
  vote_yes_count <- reactive({
    data_refresh()
    
    # Pull the latest support_rating for every voter who has any contact
    sup <- latest_field_map(con, "support_rating")
    n_contacted <- sum(sup == "1", na.rm = TRUE)
    
    # Plus DTC voters who've never been contacted (they count as "1" by default)
    n_dtc_uncontacted <- dbGetQuery(con,
                                    "SELECT COUNT(*) AS n FROM voters 
       WHERE is_dtc = 1 AND voter_id NOT IN (SELECT DISTINCT voter_id FROM contacts)")$n
    
    as.integer(n_contacted + n_dtc_uncontacted)
  })
  
  doors_knocked_count <- reactive({
    data_refresh()
    
    wt <- latest_field_map(con, "were_they_there")
    lm <- latest_field_map(con, "left_material")
    
    common <- intersect(names(wt), names(lm))
    as.integer(sum(
      !is.na(wt[common]) & lm[common] == "Yes",
      na.rm = TRUE
    ))
  })
  
  output$vote_progress <- renderUI({
    targets <- win_targets[[user_role()]]
    
    n_support <- vote_yes_count()
    pct_support <- min(100, (n_support / targets["supporters"]) * 100)
    
    n_doors <- doors_knocked_count()
    pct_doors <- min(100, (n_doors / targets["doors"]) * 100)
    
    tags$div(
      class = "progress-strip",
      style = "display: flex; flex-direction: column; gap: 4px;",
      tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$span(style = "white-space: nowrap; font-weight: bold; font-size: 0.85em; color: #FBF1D8;",
                  paste0(n_doors, " / ", format(targets["doors"], big.mark = ","), " doors knocked")),
        tags$div(style = "flex: 1; background: rgba(255,255,255,0.2); border-radius: 4px; height: 14px; overflow: hidden;",
                 tags$div(style = paste0(
                   "width: ", pct_doors, "%; height: 100%; background: #729AB9; border-radius: 4px; transition: width 0.3s ease;"
                 ))
        )
      ),
      tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$span(style = "white-space: nowrap; font-weight: bold; font-size: 0.85em; color: #FBF1D8;",
                  paste0(n_support, " / ", format(targets["supporters"], big.mark = ","), " confirmed supporters")),
        tags$div(style = "flex: 1; background: rgba(255,255,255,0.2); border-radius: 4px; height: 14px; overflow: hidden;",
                 tags$div(style = paste0(
                   "width: ", pct_support, "%; height: 100%; background: #6E120B; border-radius: 4px; transition: width 0.3s ease;"
                 ))
        )
      )
    )
  })
  
  scriptServer("script_mod", user_role)
  
  
  #end of server.
  
}



shinyApp(ui, server)
