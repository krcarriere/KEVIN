library(DBI)
library(RSQLite)
library(dplyr)

# ── connect ──────────────────────────────────────────────────────────────
con <- dbConnect(SQLite(), path.expand("~/Backups/KEVIN/kevin.sqlite"))

# ── pull raw tables ──────────────────────────────────────────────────────
contacts <- dbGetQuery(con, "SELECT * FROM contacts")
voters   <- dbGetQuery(con, "SELECT voter_id, last_name, first_name, middle_name,
                                     full_name, full_address, party_code, lat, long
                              FROM voters")

dbDisconnect(con)

# ── canonical voter_id cleanup (per your join-bug fix) ──────────────────
contacts <- contacts %>%
  mutate(voter_id = sprintf("%.0f", as.numeric(voter_id)))

voters <- voters %>%
  mutate(voter_id = sprintf("%.0f", as.numeric(voter_id)))

# ── join contacts -> voter info ──────────────────────────────────────────
knocked <- contacts %>%
  left_join(voters, by = "voter_id")

# ── quick sanity check: any contacts that didn't match a voter? ──────────
unmatched <- knocked %>% filter(is.na(full_name))
message(nrow(unmatched), " contact rows didn't match a voter (likely new_contacts / off-roll people)")

# ── de-dupe if you want ONE row per voter for mapping (e.g. latest contact) ─
knocked_latest <- knocked %>%
  group_by(voter_id) %>%
  slice_max(contact_timestamp, n = 1, with_ties = FALSE) %>%
  ungroup()






library(leaflet)

map_data <- knocked_latest %>%
  filter(!is.na(lat), !is.na(long)) |>
  filter(support_rating!="3")


leaflet(map_data) %>%
  addTiles() %>%
  addPolygons(data = mask, fillColor = "white", fillOpacity = 0.85,
              stroke = FALSE) %>%
  addPolygons(data = ns_boundary, fill = FALSE, color = "black",  weight = 2) %>%
  addCircleMarkers(lng = ~long, lat = ~lat, radius = 5, ...) %>%
  setMaxBounds(lng1 = -71.62, lat1 = 41.92, lng2 = -71.48, lat2 = 42.03) %>%
  setView(lng = -71.55, lat = 41.97, zoom = 13)

ns_boundary <- ri_places %>% 
  filter(NAME == "North Smithfield") %>%
  st_transform(4326)

leaflet(map_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat,
    radius = 5,
    color = ~case_when(
      support_rating == "1" ~ "darkgreen",
      support_rating == "2" ~ "lightgreen",
      support_rating == "4" ~ "orange",
      support_rating == "5" ~ "red",
      TRUE ~ NA
    ),
    popup = ~paste0(full_name, "<br>", full_address, "<br>", contact_date)
  ) |>
  setMaxBounds(lng1 = -71.62, lat1 = 41.92, lng2 = -71.48, lat2 = 42.03) |>
  addPolygons(data = ns_boundary, fill = FALSE, color = "black",  weight = 2)
