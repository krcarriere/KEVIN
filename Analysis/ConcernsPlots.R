library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
# adjust filename to whatever your latest backup is called
con <- dbConnect(RSQLite::SQLite(), path.expand("~/Backups/KEVIN/kevin-2026-07-02.sqlite"))

dbListTables(con)

contacts <- dbReadTable(con, "contacts") |> as_tibble()

library(tidyr)

concerns_long <- contacts |>
  select(voter_id, contact_timestamp, input_by, concern1, concern2, concern3,
         concern1_other, concern2_other, concern3_other) |>
  pivot_longer(cols = c(concern1, concern2, concern3,
                        concern1_other, concern2_other, concern3_other),
               names_to = "slot", values_to = "concern") |>
  filter(!is.na(concern), concern != "", concern != "(none)", concern!="Other") |>
  mutate(concern = case_when(
    stringr::str_detect(concern, "John") ~ "Desire for new councilors", 
    stringr::str_detect(concern, "idiots off Town") ~ "Desire for new councilors",
    stringr::str_detect(concern, "Respecting the volunteers") ~ "Respect for Boards",
    stringr::str_detect(concern, "Franklin Way") ~ "Speeding on roads",
    TRUE ~ concern))
  
bad_row1 <- concerns_long |>
  filter(concern == "Material sand and stone and too many executive sessions")
split_rows1 <- bind_rows(
  bad_row1 |> mutate(concern = "Material Sand & Stone"),
  bad_row1 |> mutate(concern = "Council transparency")
  )
bad_row2 <- concerns_long |>
  filter(concern == "Spend what you have; plan better for future")
split_rows2 <- bind_rows(
  bad_row2 |> mutate(concern = "Fiscal responsibility"),
  bad_row2 |> mutate(concern = "Budget for future")
  )
bad_row3 <- concerns_long |>
  filter(str_detect(concern, "here’s a sign on old Smithfield Road"))

split_rows3 <- bind_rows(
  bad_row2 |> mutate(concern = "Outdated road signage"),
  bad_row2 |> mutate(concern = "Halliwell"),
  bad_row2 |> mutate(concern = "Material Sand & Stone")
  )


concerns_long <- concerns_long |>
  filter(!(concern == "Material sand and stone and too many executive sessions")) |>
  filter(!(concern == "Spend what you have; plan better for future")) |>
  filter(!(str_detect(concern, "here’s a sign on old Smithfield Road"))) |>
  bind_rows(split_rows1, split_rows2, split_rows3)



library(ggplot2)

concern_counts <- concerns_long |>
  dplyr::count(concern, sort = TRUE)

concern_counts |> ggplot(aes(x = n, y = reorder(concern, n))) +
  geom_col(fill = "#50844C") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5, color = "#3A4555") +
  labs(
    x = "Mentions",
    y = NULL,
    title = "Voter Concerns"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(color = "#3A4555"),
    plot.title = element_text(face = "bold", color = "#3A4555")
  )