# ══════════════════════════════════════════════════════════════════════════
# reload_data.R — Wipe-and-reload voters + routes from pipeline CSVs.
#
# Use when the pipeline regenerates the voter file (e.g. a new district
# added). Runs on a LOCAL working copy of the DB — pull from the VPS first,
# run this, push back. Runbook is at the bottom of this file.
#
# voters and routes are pipeline-owned: nothing here is preserved. Field
# data (contacts, additional_info, new_contacts, users) lives in other
# tables and is never touched.
# ══════════════════════════════════════════════════════════════════════════

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

# ── Paths ──────────────────────────────────────────────────────────────────
voter_csv  <- "~/dev/KEVIN/ServerData/VoterData.csv"
routes_csv <- "~/dev/KEVIN/ServerData/RoutesData.csv"
dtc_path   <- "~/dev/KEVIN/Functions/dtc.R"          # source of dtc_ids

# Working copy of the DB — the file this script edits. Pull the live DB to
# this path first (see runbook). Editing a copy, never the pulled backup,
# keeps a clean fallback if anything goes wrong.
db_working <- "~/Backups/KEVIN/kevin.sqlite"

# Expected row counts — the script aborts if the CSVs read short. Bump
# these when the pipeline's universe changes.
EXPECT_VOTERS <- 40171
EXPECT_ROUTES <- 82682

# ── DB column shapes (must match the live schema) ──────────────────────────
# voters: 48 cols. 46 come straight from the CSV (after dropping the 5
# donation columns); is_dtc + may_have_moved are added below. Note `long`
# — the schema's `lng` is renamed by the ALTER step before any load.
voters_db_cols <- c(
  "voter_id","last_name","first_name","middle_name","full_name",
  "street_number","street_name","suffix_a","suffix_b","unit","unit_num",
  "zip_code","zip4_code","zero_zip_code","city","state","full_address",
  "street","party_code","effective_date","gender","registration_date",
  "date_made_inactive","state_senate_district","state_rep_district",
  "precinct","ward_council","ward_district","general_score","primary_score",
  "turnout_score","primary_2024","primary_2022","primary_2020",
  "general_2024","general_2022","general_2020","nsse_2026_voter",
  "voter_status","age","lat","long","times_spoken","donate_text",
  "additional","who_knows","is_dtc","may_have_moved"
)

routes_db_cols <- c(
  "voter_id","scenario","precinct","full_address",
  "voters_at_address","route","stop_order","route_name"
)

# Pipeline columns with no home in the voters schema — dropped on load.
donation_cols <- c("n_donations_Fed","committees_donated_Fed",
                   "n_donations_RI","committees_donated_RI","TotalDonated")

# ── Load dtc_ids ───────────────────────────────────────────────────────────
# is_dtc is re-derived from this constant rather than trusted from the CSV
# — dtc.R is the unambiguous source of truth. If the pipeline ever starts
# emitting is_dtc, dtc.R still wins (the mutate below overwrites it).
source(path.expand(dtc_path))
stopifnot("dtc.R did not define dtc_ids" = exists("dtc_ids"))

# ── Read the CSVs ──────────────────────────────────────────────────────────
# voter_id forced to character (11-digit string, TEXT primary key); zip
# columns forced to character (RI zips have leading zeros).
voters_in <- read.csv(path.expand(voter_csv), stringsAsFactors = FALSE,
                      colClasses = c(voter_id = "character", zip_code = "character",
                                     zip4_code = "character", zero_zip_code = "character"))
routes_in <- read.csv(path.expand(routes_csv), stringsAsFactors = FALSE,
                      colClasses = c(voter_id = "character"))

# ── Shape voters ───────────────────────────────────────────────────────────
voters_df <- voters_in |>
  select(-any_of(donation_cols)) |>
  mutate(
    may_have_moved = 0L,
    is_dtc = if_else(voter_id %in% as.character(dtc_ids), 1L, 0L)
  ) |>
  select(all_of(voters_db_cols))   # exact schema order; errors loudly if a col is missing

# ── Shape routes ───────────────────────────────────────────────────────────
# Pipeline routes CSV carries extra columns (X, n_stops, dist_miles,
# timings, Sheet, Date) — select() keeps only the 8 schema columns.
routes_df <- routes_in |>
  select(all_of(routes_db_cols)) |>
  mutate(
    precinct          = as.integer(precinct),
    voters_at_address = as.integer(voters_at_address),
    route             = as.integer(route),
    stop_order        = as.integer(stop_order)
  )

# ── Pre-flight checks (before touching the DB) ─────────────────────────────
stopifnot(
  "voter CSV row count unexpected"  = nrow(voters_df) == EXPECT_VOTERS,
  "routes CSV row count unexpected" = nrow(routes_df) == EXPECT_ROUTES,
  "duplicate voter_id in voter CSV" = !any(duplicated(voters_df$voter_id))
)
message(sprintf("Pre-flight OK — %d voters, %d routes.",
                nrow(voters_df), nrow(routes_df)))

# ── Open the working DB ────────────────────────────────────────────────────
con <- dbConnect(SQLite(), path.expand(db_working))
on.exit(dbDisconnect(con), add = TRUE)

# ── ALTER: rename lng -> long (idempotent) ─────────────────────────────────
cols_now <- dbListFields(con, "voters")
if ("lng" %in% cols_now && !("long" %in% cols_now)) {
  dbExecute(con, "ALTER TABLE voters RENAME COLUMN lng TO long")
  message("Renamed voters.lng -> voters.long")
} else if ("long" %in% cols_now) {
  message("voters.long already present — skipping rename.")
} else {
  stop("voters table has neither 'lng' nor 'long' — schema mismatch, aborting.")
}

# ── Wipe + reload, inside one transaction ──────────────────────────────────
# dbWithTransaction rolls back automatically if anything in the block
# errors — including the count check below. Either both tables reload
# cleanly, or the DB is left exactly as it was.
dbWithTransaction(con, {
  dbExecute(con, "DELETE FROM voters")
  dbExecute(con, "DELETE FROM routes")
  dbAppendTable(con, "voters", voters_df)
  dbAppendTable(con, "routes", routes_df)
  
  n_v <- dbGetQuery(con, "SELECT COUNT(*) n FROM voters")$n
  n_r <- dbGetQuery(con, "SELECT COUNT(*) n FROM routes")$n
  stopifnot(
    "voters row count wrong after load" = n_v == EXPECT_VOTERS,
    "routes row count wrong after load" = n_r == EXPECT_ROUTES
  )
  message(sprintf("Loaded — voters: %d, routes: %d. Committing.", n_v, n_r))
})

# ── Checkpoint WAL so the pushed file is self-contained ────────────────────
dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)")
message("Done. Working DB ready to push: ", path.expand(db_working))

# ══════════════════════════════════════════════════════════════════════════
# RUNBOOK — pull, reload, push. Run with the app stopped so no live writes
# are lost (pull-modify-push overwrites whatever is on the server).
#
#   VPS=user@67.205.173.18
#   LIVE=/srv/shiny-server/kevin/ServerData/kevin.sqlite
#
#   # 1. Stop the app — closes connections, checkpoints WAL into the file
#   ssh $VPS 'sudo systemctl stop shiny-server'
#
#   # 2. Pull: a timestamped safety backup, then the working copy
#   scp $VPS:$LIVE ~/Backups/KEVIN/kevin_$(date +%Y%m%d_%H%M).sqlite
#   scp $VPS:$LIVE ~/Backups/KEVIN/kevin_working.sqlite
#
#   # 3. Reload (this script)
#   Rscript ~/dev/KEVIN/ServerData/reload_data.R
#
#   # 4. Push the working copy back; clear stale WAL/SHM on the server
#   scp ~/Backups/KEVIN/kevin.sqlite $VPS:$LIVE
#   ssh $VPS "rm -f ${LIVE}-wal ${LIVE}-shm"
#
#   # 5. Restart — app.R reloads voters_raw/routes_raw at startup
#   ssh $VPS 'sudo systemctl start shiny-server'
# ══════════════════════════════════════════════════════════════════════════