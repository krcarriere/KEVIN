# Functions/mod_script.R

scriptUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("script_content"))
}

scriptServer <- function(id, user_role) {
  moduleServer(id, function(input, output, session) {
    
    script_view <- reactiveVal("own")
    
    observe({
      user_role()
      script_view("own")
    })
    
    observeEvent(input$toggle_script, {
      script_view(if (script_view() == "own") "other" else "own")
    })
    
    output$script_content <- renderUI({
      role <- user_role()
      
      if (role %in% c("ns", "admin", "sb")) {
        show_tc <- (role %in% c("ns", "admin") && script_view() == "own") ||
          (role == "sb" && script_view() == "other")
        
        tc_script <- tagList(
          tags$h5("United for North Smithfield \u2014 Town Council"),
          tags$p(style = "font-style: italic; color: #555;",
                 "Paul Jones \u2022 Kevin Carriere \u2022 Ryan Hammond"),
          tags$hr(),
          tags$p(tags$strong("Opening:")),
          tags$p("Hi, my name is ___ and I\u2019m volunteering with the North Smithfield Forward Coalition. ",
                 "We\u2019re a group of candidates running together for Town Council and School Committee ",
                 "because we think North Smithfield deserves competent, transparent leadership ",
                 "that plans for the future without pricing out the people who made this town great."),
          tags$p(tags$strong("Engage:")),
          tags$p("Do you have a minute? We\u2019re talking to neighbors to hear what matters most to you \u2014 ",
                 "things like taxes, the roads, schools, the police station project, anything."),
          tags$p(tags$em(style = "color: #555;",
                         "Listen. Let them talk. Record their concerns in the app.")),
          tags$hr(),
          tags$p(tags$strong("Respond with the platform:")),
          tags$p("Tailor your response to what they raised. Pull from these three pillars:"),
          tags$p(tags$strong(style = "color: #003B4F;", "1. Ethics & Transparency")),
          tags$p("\u2014 Full oversight on the $9M police station renovation with public updates on spending. ",
                 "Enforce local ordinances \u2014 the quarry, the junkyard \u2014 consistently. ",
                 "A real code of conduct with actual enforcement."),
          tags$p(tags$strong(style = "color: #003B4F;", "2. Smart Investment")),
          tags$p("\u2014 Move the Halliwell Community Center forward. Bring water and sewer to the 146 corridor ",
                 "to attract small business. Fix the roads \u2014 lobby the state hard on repaving. ",
                 "Build relationships with state leaders to bring resources home."),
          tags$p(style = "font-size: 0.9em; color: #555;",
                 "Our School Committee candidates are also committed to protecting the schools ",
                 "that make this town attractive \u2014 top-tier Superintendent, no cuts that hurt quality."),
          tags$p(tags$strong(style = "color: #003B4F;", "3. Affordability")),
          tags$p("\u2014 Grow the commercial tax base so homeowners aren\u2019t carrying everything. ",
                 "Proactive planning instead of last-minute spending. ",
                 "Protect seniors from being priced out of their homes."),
          tags$hr(),
          tags$p(tags$strong("Ask:")),
          tags$p("Does that sound like the kind of leadership you\u2019d support? ",
                 "Can we count on your vote for our slate in November?"),
          tags$p(tags$strong("Close:")),
          tags$p("Thank you so much for your time. Have a great day."),
          tags$hr(),
          tags$p(tags$strong("If not home:")),
          tags$p("Leave door tag. Mark \u201CNo\u201D for \u201CWere they there?\u201D and \u201CYes\u201D for \u201CLeft material.\u201D")
        )
        
        sc_script <- tagList(
          tags$h5("United for North Smithfield \u2014 School Committee"),
          tags$p(style = "font-style: italic; color: #555;",
                 "Gabrielle Stelle \u2022 Mark Fasano"),
          tags$hr(),
          tags$p(tags$strong("Opening:")),
          tags$p("Hi, my name is ___ and I\u2019m volunteering with the North Smithfield Forward Coalition. ",
                 "We\u2019re a group of candidates running together for School Committee and Town Council ",
                 "because we believe North Smithfield\u2019s schools are one of the biggest reasons ",
                 "people move here \u2014 and stay \u2014 and they deserve real investment and protection."),
          tags$p(tags$strong("Engage:")),
          tags$p("Do you have a minute? We\u2019re talking to neighbors about what matters most \u2014 ",
                 "the schools, the budget, what you\u2019d like to see for kids in this town."),
          tags$p(tags$em(style = "color: #555;",
                         "Listen. Let them talk. Record their concerns in the app.")),
          tags$hr(),
          tags$p(tags$strong("Respond with the platform:")),
          tags$p(tags$strong(style = "color: #003B4F;", "Education as an Asset")),
          tags$p("\u2014 Previous generations built a school system to be proud of. ",
                 "We\u2019ll honor that by hiring a top-tier Superintendent and resisting cuts ",
                 "that would hurt test scores and, ultimately, the resale value of every home in town."),
          tags$p(tags$strong(style = "color: #003B4F;", "Accountability")),
          tags$p("\u2014 Transparent budgeting, clear communication with parents and taxpayers, ",
                 "and a School Committee that works with the Town Council \u2014 not against it."),
          tags$p(tags$strong(style = "color: #003B4F;", "The Full Slate")),
          tags$p(style = "font-size: 0.9em; color: #555;",
                 "Our Town Council candidates are fighting for smart infrastructure \u2014 ",
                 "the Halliwell Center, roads, growing the commercial tax base to take pressure ",
                 "off homeowners. A stronger town means a stronger school system."),
          tags$hr(),
          tags$p(tags$strong("Ask:")),
          tags$p("Does that sound like the kind of leadership you\u2019d support? ",
                 "Can we count on your vote for our slate in November?"),
          tags$p(tags$strong("Close:")),
          tags$p("Thank you so much for your time. Have a great day."),
          tags$hr(),
          tags$p(tags$strong("If not home:")),
          tags$p("Leave door tag. Mark \u201CNo\u201D for \u201CWere they there?\u201D and \u201CYes\u201D for \u201CLeft material.\u201D")
        )
        
        tagList(
          tags$div(style = "text-align: right;",
                   actionLink(session$ns("toggle_script"),
                              label = if (show_tc) "View School Committee script \u25B6"
                              else "\u25C0 View Town Council script")
          ),
          if (show_tc) tc_script else sc_script
        )
        
      } else if (role == "d17") {
        tagList(
          tags$h5("Nelly for State Senate \u2014 District 17"),
          tags$hr(),
          tags$p(tags$strong("Intro:")),
          tags$p("Hi, my name is ___ and I\u2019m a volunteer for Nelly\u2019s campaign for State Senate in District 17."),
          tags$p(tags$strong("Engage:")),
          tags$p("Do you have a minute to chat? Nelly is running because [candidate\u2019s why]. ",
                 "We\u2019re out talking to voters to hear what matters most to you."),
          tags$p(tags$em(style = "color: #555;", "Record their top concerns in the app.")),
          tags$p(tags$strong("Ask:")),
          tags$p("Nelly\u2019s priorities include [key platform items]. Does that resonate with you?"),
          tags$p(tags$strong("Close:")),
          tags$p("Thanks for your time. Can we count on you to vote for Nelly in November?"),
          tags$hr(),
          tags$p(class = "text-muted", tags$em("Update with Nelly\u2019s specific platform when available."))
        )
      } else if (role == "d49") {
        tagList(
          tags$h5("Veronicka for District 49"),
          tags$hr(),
          tags$p(tags$strong("Intro:")),
          tags$p("Hi, my name is ___ and I\u2019m a volunteer for Veronicka\u2019s campaign for District 49."),
          tags$p(tags$strong("Engage:")),
          tags$p("Do you have a minute? Veronicka is running because [candidate\u2019s why]. ",
                 "We\u2019re talking to neighbors to hear what\u2019s on your mind."),
          tags$p(tags$em(style = "color: #555;", "Record their top concerns in the app.")),
          tags$p(tags$strong("Ask:")),
          tags$p("Tyler\u2019s focused on [key platform items]. Is that something you care about too?"),
          tags$p(tags$strong("Close:")),
          tags$p("Thanks so much. Can we count on you to vote for Veronicka in the primary?"),
          tags$hr(),
          tags$p(class = "text-muted", tags$em("Update with Veronicka\u2019s specific platform when available."))
        )
      } else if (role == "paw") {
        tagList(
          tags$h5("Tyler for Pawtucket City Council"),
          tags$hr(),
          tags$p(tags$strong("Intro:")),
          tags$p("Hi, my name is ___ and I\u2019m a volunteer for Tyler\u2019s campaign for City Council here in Pawtucket."),
          tags$p(tags$strong("Engage:")),
          tags$p("Do you have a minute? Tyler is running because [candidate\u2019s why]. ",
                 "We\u2019re talking to neighbors to hear what\u2019s on your mind."),
          tags$p(tags$em(style = "color: #555;", "Record their top concerns in the app.")),
          tags$p(tags$strong("Ask:")),
          tags$p("Tyler\u2019s focused on [key platform items]. Is that something you care about too?"),
          tags$p(tags$strong("Close:")),
          tags$p("Thanks so much. Can we count on you to vote for Tyler in the primary?"),
          tags$hr(),
          tags$p(class = "text-muted", tags$em("Update with Tyler\u2019s specific platform when available."))
        )
        
      } else {
        tags$p(class = "text-muted", "No script available for this role.")
      }
    })
  })
}