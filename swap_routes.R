# swap_routes.R  — run locally first, pointing at a COPY
library(DBI); library(RSQLite); library(dplyr)

db  <- path.expand("~/Backups/KEVIN/kevin.sqlite")          # a copy, not live
csv <- path.expand("~/dev/KEVIN/ServerData/RoutesData.csv")

con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
dbExecute(con, "PRAGMA foreign_keys = ON")

tbl_cols <- dbGetQuery(con, "PRAGMA table_info(routes)")$name

new <- read.csv(csv, stringsAsFactors = FALSE, check.names = TRUE)
new <- new[, !grepl("^X$|^X\\.", names(new)), drop = FALSE]   # drop write.csv row index

message("MISSING from CSV (→ NULL): ", paste(setdiff(tbl_cols, names(new)), collapse=", "))
message("EXTRA in CSV (dropped): ",    paste(setdiff(names(new), tbl_cols), collapse=", "))

new <- new |>
  mutate(across(any_of(c("precinct","route","stop_order","n_stops",
                         "voters_at_address","Sheet")), as.integer)) |>
  select(any_of(tbl_cols))

scn <- sort(unique(new$scenario)); message("Scenarios in CSV: ", paste(scn, collapse=", "))

dbWithTransaction(con, {
  dbExecute(con, paste0("DELETE FROM routes WHERE scenario IN (",
                        paste(sprintf("'%s'", scn), collapse=","), ")"))
  dbAppendTable(con, "routes", new)
})

dbGetQuery(con, "SELECT scenario, COUNT(*) n FROM routes GROUP BY scenario ORDER BY scenario")