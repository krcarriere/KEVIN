# ══════════════════════════════════════════════════════════════════════════
# Logins.R — DB-backed credentials
#
# Reads the `users` table from kevin.sqlite instead of hardcoding hashes.
# The table is created by migrate_users_table.R (run once).
#
# Exports:
#   load_user_base(con)  — returns a tibble with user, password, role
#                          (same shape shinyauthr expects)
# ══════════════════════════════════════════════════════════════════════════

load_user_base <- function(con) {
  DBI::dbGetQuery(con,
    "SELECT username AS user, password_hash AS password, role, precinct FROM users"
  ) |> tibble::as_tibble()
}
