# ══════════════════════════════════════════════════════════════════════════
# db_helpers.R — replaces grabRecentData.R
#
# Read-side helpers are dbplyr-based (write dplyr, it turns into SQL).
# Write-side helpers use DBI::dbExecute with parameterized SQL — there
# are only a handful of them, one per use case.
# ══════════════════════════════════════════════════════════════════════════

library(DBI)
library(RSQLite)
library(dplyr)
library(dbplyr)

# ── connection ────────────────────────────────────────────────────────────
open_db <- function(path = "ServerData/kevin.sqlite") {
  con <- dbConnect(SQLite(), path)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  dbExecute(con, "PRAGMA journal_mode = WAL")
  con
}

close_db <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
}

# ══════════════════════════════════════════════════════════════════════════
# READ HELPERS
# ══════════════════════════════════════════════════════════════════════════

# Whether a voter has a turnout record for a given election. Live query —
# reflects the most recent voter_turnout import with no restart needed.
voted_in_election <- function(con, vid, election) {
  n <- DBI::dbGetQuery(con,
                       "SELECT COUNT(*) AS n FROM voter_turnout WHERE voter_id = ? AND election = ?",
                       params = list(vid, election))$n
  isTRUE(n > 0)
}

# Whether a voter has a mail ballot request on file for a given election.
requested_mail_ballot <- function(con, vid, election) {
  n <- DBI::dbGetQuery(con,
                       "SELECT COUNT(*) AS n FROM voter_mail_ballot WHERE voter_id = ? AND election = ?",
                       params = list(vid, election))$n
  isTRUE(n > 0)
}

# Latest contact row for one voter (0 or 1 row returned).
latest_contact <- function(con, vid) {
  tbl(con, "contacts") |>
    filter(voter_id == !!as.character(vid)) |>
    arrange(desc(contact_timestamp)) |>
    head(1) |>
    collect()
}

# Named character vector: voter_id (as chr) → latest non-empty value of `field`.
# Replaces latest_col_vec's row-wise scan.
latest_field_map <- function(con, field) {
  allowed <- c("support_rating", "contact_timestamp", "left_material",
               "were_they_there", "had_conversation", "campaigner",
               "contact_type", "concern1", "concern2", "concern3", "input_by")
  stopifnot(field %in% allowed)
  
  q <- tbl(con, "contacts") |>
    filter(!is.na(.data[[field]]), .data[[field]] != "") |>
    select(voter_id, contact_timestamp, all_of(field)) |>
    collect() |>
    group_by(voter_id) |>
    slice_max(contact_timestamp, n = 1, with_ties = FALSE) |>
    ungroup()
  
  setNames(q[[field]], as.character(q$voter_id))
}

# Voters × routes for a scenario — equivalent of voter_data()'s `main` set
# before off-route are bound in. Also includes voter-level flags
# (may_have_moved, is_dtc, who_knows, donate_text, additional,
# times_spoken) because the app reads those directly.
voter_scenario <- function(con, scenario_name) {
  tbl(con, "voters") |>
    inner_join(
      tbl(con, "routes") |> filter(scenario == !!scenario_name),
      by = c("voter_id", "precinct")
    ) |>
    collect()
}

# Off-route voters = voters in a scenario's precincts but not in routes
voters_off_route <- function(con, scenario_name) {
  scen_precincts <- tbl(con, "routes") |>
    filter(scenario == !!scenario_name) |>
    distinct(precinct) |>
    pull(precinct)

  routed_ids <- tbl(con, "routes") |>
    filter(scenario == !!scenario_name) |>
    distinct(voter_id) |>
    pull(voter_id)

  if (length(scen_precincts) == 0) {
    return(tibble())
  }

  tbl(con, "voters") |>
    filter(precinct %in% !!scen_precincts,
           !voter_id %in% !!routed_ids) |>
    collect() |>
    mutate(
      route      = NA_integer_,
      stop_order = NA_integer_,
      scenario   = !!scenario_name,
      route_name = NA_character_
    )
}

# Voters who have any contact row at all (used for "Recorded" indicator)
voters_with_any_contact <- function(con) {
  tbl(con, "contacts") |>
    distinct(voter_id) |>
    pull(voter_id)
}

# ══════════════════════════════════════════════════════════════════════════
# WRITE HELPERS
# ══════════════════════════════════════════════════════════════════════════

# Insert one contact row. `fields` is a named list of optional columns.
# Clears may_have_moved on the voter as a side effect — a successful
# contact is the signal that the voter hasn't moved.
insert_contact <- function(con, vid, input_by, fields = list()) {
  now_ts   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  now_date <- format(Sys.Date(), "%Y-%m-%d")

  allowed <- c("campaigner", "contact_type", "left_material", "were_they_there",
               "had_conversation", "support_rating",
               "concern1", "concern1_other",
               "concern2", "concern2_other",
               "concern3", "concern3_other",
               "notes")
  f <- fields[intersect(names(fields), allowed)]
  # Convert "" → NA so NULLs land in the DB, not empty strings
  f <- lapply(f, function(x) {
    if (is.null(x)) NA_character_
    else if (is.character(x) && !nzchar(trimws(x))) NA_character_
    else x
  })

  cols <- c("voter_id", "contact_date", "contact_timestamp", "input_by", names(f))
  vals <- c(list(as.character(vid), now_date, now_ts, input_by), f)

  placeholders <- paste(rep("?", length(cols)), collapse = ", ")
  col_list     <- paste(cols, collapse = ", ")

  dbExecute(con,
    paste0("INSERT INTO contacts (", col_list, ") VALUES (", placeholders, ")"),
    params = unname(vals))

  # Clear may_have_moved — a recorded contact = this person is still here.
  dbExecute(con,
    "UPDATE voters SET may_have_moved = 0 WHERE voter_id = ?",
    params = list(as.character(vid)))

  invisible(TRUE)
}

# Flag a batch of voters as may_have_moved. Called from the new-contact
# save path to flag everyone currently at that door.
flag_may_have_moved <- function(con, voter_ids) {
  if (length(voter_ids) == 0) return(invisible(FALSE))
  ids <- as.character(voter_ids)
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) return(invisible(FALSE))
  placeholders <- paste(rep("?", length(ids)), collapse = ", ")
  dbExecute(con,
    paste0("UPDATE voters SET may_have_moved = 1 WHERE voter_id IN (",
           placeholders, ")"),
    params = as.list(ids))
  invisible(TRUE)
}

voter_left_material_since <- function(con, vid, cutoff_date = "2026-07-07") {
  n <- tbl(con, "contacts") |>
    filter(voter_id == !!as.character(vid),
           left_material == "Yes",
           contact_date > !!cutoff_date) |>
    summarise(n = n()) |>
    pull(n)
  isTRUE(n > 0)
}

# Record a person we met at the door who wasn't on the roll.
# Atomic: writes new_contacts, voters stub, and a contacts row together.
# Returns the newly allocated voter_id.
insert_new_contact <- function(con, recorded_by,
                                first_name, last_name, address,
                                precinct, route, stop_order, scenario,
                                interaction_fields = list()) {
  max_id <- DBI::dbGetQuery(con, "SELECT MAX(CAST(voter_id AS INTEGER)) AS m FROM new_contacts")$m
  new_vid <- as.character(if (is.na(max_id)) 11111L else as.integer(max_id) + 1L)
  now_ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  dbWithTransaction(con, {
    dbExecute(con,
      "INSERT INTO new_contacts
       (voter_id, first_name, last_name, address, precinct, route, stop_order,
        scenario, recorded_by, timestamp)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(new_vid, first_name, last_name, address,
                    as.integer(precinct), as.integer(route), as.integer(stop_order),
                    scenario, recorded_by, now_ts))

    dbExecute(con,
      "INSERT INTO voters
       (voter_id, first_name, last_name, full_name, full_address, street,
        precinct, is_dtc, may_have_moved)
       VALUES (?, ?, ?, ?, ?, ?, ?, 0, 0)",
      params = list(new_vid, first_name, last_name,
                    trimws(paste(first_name, last_name)),
                    address, address, as.integer(precinct)))

    insert_contact(con, new_vid, recorded_by, interaction_fields)
  })

  new_vid
}

# AdditionalInfo flow.

insert_additional_info <- function(con, vid, who_knows, additional_text, updated_by) {
  
  # Coerce each text param to a safe single-length character (NA_character_ for empty/NA/NULL).
  # nzchar(NA) returns NA in current R, so is.na() must come BEFORE nzchar() — same
  # pattern as append_who_knows() below.
  clean_text <- function(x, trim = FALSE) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    x <- as.character(x)
    if (is.na(x)) return(NA_character_)
    if (trim) x <- trimws(x)
    if (!nzchar(x)) return(NA_character_)
    x
  }
  
  vid_val        <- clean_text(vid)
  who_val        <- clean_text(who_knows)
  text_val       <- clean_text(additional_text, trim = TRUE)
  updated_by_val <- clean_text(updated_by)
  
  # vid and updated_by are required — refuse to write a useless row.
  if (is.na(vid_val) || is.na(updated_by_val)) {
    warning("insert_additional_info: missing vid or updated_by; skipping insert.")
    return(invisible(FALSE))
  }
  
  dbExecute(con,
            "INSERT INTO additional_info
     (voter_id, who_knows, additional_text, updated_by, timestamp)
     VALUES (?, ?, ?, ?, ?)",
            params = list(vid_val,
                          who_val,
                          text_val,
                          updated_by_val,
                          format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  invisible(TRUE)
}

# Append an initial to voter's who_knows column, deduplicating.
append_who_knows <- function(con, vid, initial) {
  if (is.null(initial) || is.na(initial) || !nzchar(initial)) return(invisible(FALSE))
  current <- dbGetQuery(con,
    "SELECT who_knows FROM voters WHERE voter_id = ?",
    params = list(as.character(vid)))$who_knows

  parts <- if (length(current) == 0 || is.na(current) || !nzchar(current)) {
    character(0)
  } else {
    strsplit(current, ",\\s*")[[1]]
  }
  parts <- unique(c(parts, initial))
  parts <- parts[nzchar(parts)]
  new_val <- paste(parts, collapse = ", ")

  dbExecute(con,
    "UPDATE voters SET who_knows = ? WHERE voter_id = ?",
    params = list(new_val, as.character(vid)))
  invisible(TRUE)
}

# Append to voter's additional-text field (semicolon-separated history).
append_additional <- function(con, vid, text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return(invisible(FALSE))
  current <- dbGetQuery(con,
    "SELECT additional FROM voters WHERE voter_id = ?",
    params = list(as.character(vid)))$additional
  new_val <- if (length(current) == 0 || is.na(current) || !nzchar(current)) {
    trimws(text)
  } else {
    paste0(current, "; ", trimws(text))
  }
  dbExecute(con,
    "UPDATE voters SET additional = ? WHERE voter_id = ?",
    params = list(new_val, as.character(vid)))
  invisible(TRUE)
}

# Fetch all additional_info notes for one voter, collapsed into a single
# display string. Returns "" if none exist. Pipeline-owned
# voters.additional (e.g. donation text baked in by the pipeline) is NOT
# touched — that stays on voters and displays separately if needed.
get_additional_notes <- function(con, vid) {
  rows <- DBI::dbGetQuery(con,
                          "SELECT additional_text, updated_by, timestamp
     FROM additional_info
     WHERE voter_id = ? AND additional_text IS NOT NULL
     ORDER BY timestamp",
                          params = list(as.character(vid)))
  
  if (nrow(rows) == 0) return("")
  
  # Format: "note text (K, 04/20)" for each entry
  parts <- mapply(function(txt, who, ts) {
    ts_short <- format(as.POSIXct(ts), "%m/%d")
    who_short <- if (!is.na(who) && nzchar(who)) who else "?"
    paste0(txt, " (", who_short, ", ", ts_short, ")")
  }, rows$additional_text, rows$updated_by, rows$timestamp,
  USE.NAMES = FALSE)
  
  paste(parts, collapse = "; ")
}

# Write Helper
validate_choice <- function(input_value, allowed) {
  if (is.null(input_value)) return(NA_character_)
  if (input_value %in% allowed) return(input_value)
  NA_character_
}


# Organizing Data
get_same_door_voters <- function(v, rd) {
  rd |>
    dplyr::filter(
      Precinct  == v$Precinct,
      Route     == v$Route,
      Street == v$Street
    ) |>
    dplyr::arrange(LastName, FirstName)
}

voted_general_ids <- function(con) {
  dbGetQuery(con, "SELECT voter_id FROM voter_turnout WHERE election = 'General2026'")$voter_id
}



get_address_signs <- function(con, full_address) {
  if (is.null(full_address) || !nzchar(full_address)) return(character(0))
  dbGetQuery(con,
             "SELECT sign_code FROM address_signs WHERE full_address = ?",
             params = list(full_address)
  )$sign_code
}

set_address_signs <- function(con, full_address, sign_names, updated_by) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dbWithTransaction(con, {
    dbExecute(con, "DELETE FROM address_signs WHERE full_address = ?",
              params = list(full_address))
    for (nm in sign_names) {
      dbExecute(con,
                "INSERT INTO address_signs (full_address, sign_code, updated_by, updated_at)
         VALUES (?, ?, ?, ?)",
                params = list(full_address, nm, updated_by, ts))
    }
  })
}
