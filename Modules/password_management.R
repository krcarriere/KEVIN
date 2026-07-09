# ══════════════════════════════════════════════════════════════════════════
# password_management.R
#
# Shiny module for password changes:
#   • Forced password change on first login (must_change_pw = 1)
#   • Voluntary password change via a button in the sidebar
#   • Server-side validation: 8+ chars, uppercase, lowercase, digit, symbol
#   • Writes sodium::password_store() hash to the users table
#
# Usage in app.R:
#   source("Modules/passwordManagement.R")
#
#   # In UI — place the button somewhere visible after login:
#   passwordChangeButtonUI("pw_mod")
#
#   # In server:
#   passwordChangeServer("pw_mod",
#     credentials  = credentials,
#     con          = con
#   )
# ══════════════════════════════════════════════════════════════════════════

# ── Password validation ──────────────────────────────────────────────────
# Returns NULL if valid, or a character string describing the problem.
validate_password <- function(pw) {
  if (is.null(pw) || !nzchar(pw))
    return("Password cannot be empty.")
  if (nchar(pw) < 8)
    return("Password must be at least 8 characters.")
  if (!grepl("[A-Z]", pw))
    return("Password must contain at least one uppercase letter.")
  if (!grepl("[a-z]", pw))
    return("Password must contain at least one lowercase letter.")
  if (!grepl("[0-9]", pw))
    return("Password must contain at least one number.")
  if (!grepl("[^A-Za-z0-9]", pw))
    return("Password must contain at least one special character (!@#$%^&* etc.).")
  NULL
}

# ── DB helpers ────────────────────────────────────────────────────────────

# Check whether this user must change their password before proceeding.
user_must_change_pw <- function(con, username) {
  res <- DBI::dbGetQuery(con,
    "SELECT must_change_pw FROM users WHERE username = ?",
    params = list(username))
  if (nrow(res) == 0) return(FALSE)
  isTRUE(res$must_change_pw == 1)
}

# Write a new password hash and clear the must_change_pw flag.
update_user_password <- function(con, username, new_hash) {
  DBI::dbExecute(con,
    "UPDATE users SET password_hash = ?, must_change_pw = 0
     WHERE username = ?",
    params = list(new_hash, username))
  invisible(TRUE)
}

# ── UI: small button for voluntary password change ────────────────────────
password_change_button_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionLink(ns("open_change_pw"), 
                    label = shiny::tags$span(
                      style = "font-size: 0.85em; cursor: pointer;",
                      bsicons::bs_icon("key-fill"), " Change Password"
                    ))
}

# ── Server module ─────────────────────────────────────────────────────────
password_change_server <- function(id, credentials, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Forced change on first login ──────────────────────────────────────
    # This fires once when credentials become valid. If must_change_pw = 1,
    # we show an unclosable modal.
    shiny::observeEvent(credentials(), {
      req(credentials()$user_auth)
      username <- credentials()$info$user
      if (user_must_change_pw(con, username)) {
        show_change_modal(ns, forced = TRUE)
      }
    })

    # ── Voluntary change ──────────────────────────────────────────────────
    shiny::observeEvent(input$open_change_pw, {
      show_change_modal(ns, forced = FALSE)
    })

    # ── Save handler ──────────────────────────────────────────────────────
    shiny::observeEvent(input$save_new_pw, {
      username <- credentials()$info$user

      # For forced changes, we don't ask for the old password (they may
      # have been given a temporary one by Kevin and we just want them to
      # set something they know). For voluntary changes, verify the old
      # password first.
      is_forced <- user_must_change_pw(con, username)

      if (!is_forced) {
        old_pw <- input$old_pw %||% ""
        if (!nzchar(old_pw)) {
          shiny::showNotification("Enter your current password.", type = "error")
          return()
        }
        # Look up current hash
        current_hash <- DBI::dbGetQuery(con,
          "SELECT password_hash FROM users WHERE username = ?",
          params = list(username))$password_hash
        if (length(current_hash) == 0 ||
            !sodium::password_verify(current_hash, old_pw)) {
          shiny::showNotification("Current password is incorrect.", type = "error")
          return()
        }
      }

      new_pw  <- input$new_pw  %||% ""
      conf_pw <- input$conf_pw %||% ""

      # Validate new password
      err <- validate_password(new_pw)
      if (!is.null(err)) {
        shiny::showNotification(err, type = "error")
        return()
      }

      if (new_pw != conf_pw) {
        shiny::showNotification("Passwords don't match.", type = "error")
        return()
      }

      # Hash and save
      new_hash <- sodium::password_store(new_pw)
      update_user_password(con, username, new_hash)

      shiny::removeModal()
      shiny::showNotification("Password updated!", type = "message")
    })
  })
}

# ── Modal builder (shared by forced + voluntary flows) ────────────────────
show_change_modal <- function(ns, forced = FALSE) {
  shiny::showModal(shiny::modalDialog(
    title = if (forced) "Set Your Password" else "Change Password",

    if (forced) {
      shiny::tags$p(class = "text-muted",
        "Welcome! Please set a password you'll remember. ",
        "You won't be able to use the app until you do.")
    } else {
      shiny::passwordInput(ns("old_pw"), "Current password")
    },

    shiny::passwordInput(ns("new_pw"), "New password"),
    shiny::passwordInput(ns("conf_pw"), "Confirm new password"),

    shiny::tags$p(class = "text-muted", style = "font-size: 0.85em;",
      "At least 8 characters, with uppercase, lowercase, a number, and a special character."
    ),

    footer = shiny::tagList(
      if (!forced) shiny::modalButton("Cancel"),
      shiny::actionButton(ns("save_new_pw"), "Save Password", class = "btn-primary")
    ),
    size = "s",
    easyClose = !forced   # forced modal can't be dismissed
  ))
}
