
concern_choices <- c(
  "(none)",
  "Cost of Living",
  "Taxes / Property Tax",
  "Environment / Conservation",
  "Material Sand & Stone",
  "Junk Yard",
  "Tree Cutting",
  "Roads / Infrastructure",
  "Clean Water",
  "Sewer Lines",
  "146 Economic Expansion",
  "Schools / Education",
  "Police Station / Public Safety",
  "Transparency / Accountability of TC",
  "Respect for Boards",
  "Delay of decisions",
  "Senior Services",
  "Halliwell / Scouter's Hall",
  "Pacheco Park",
  "Zoning / Development",
  "Small Businesses",
  "Community Growth",
  "Other"
)

validators <- list(
  campaigner_choice = c("Paul", "Kevin", "Gabbie", "Mark", "Nelly", "Tyler", "Veronicka"),
  left_material_choice = c("Yes", "No"),
  were_they_there_choice = c("Yes", "No"),
  had_conversation_choice = c("Yes", "No"),
  support_rating_choice = c("1", "2", "3", "4", "5"),
  contact_type_choice = c("knock", "email"),
  concern1_choice = concern_choices, 
  concern2_choice = concern_choices, 
  concern3_choice = concern_choices,
  history_elections = c("Primary2020", "General2020", "Primary2022",
                        "General2022", "Primary2024", "General2024")
)

history_map <- c(
  "Primary 2020" = "Primary2020",
  "General 2020" = "General2020",
  "Primary 2022" = "Primary2022",
  "General 2022" = "General2022",
  "Primary 2024" = "Primary2024",
  "General 2024" = "General2024"
)

user_campaigner <- function(user) {
  mapping <- c("pjones" = "Paul", 
               "kcarriere" = "Kevin",
               "rhammond" = "Ryan",
               
               
               "gstelle" = "Gabbie", 
               "mfasano" = "Mark", 
               "afasano" = "Mark",
               
               "nburdette" = "Nelly",
               "wburdette"  = "Nelly",
               
               "tmcfeeters"  = "Tyler",
               "jmcfeeters"  = "Tyler",
               
               "vvega"  = "Veronicka"
               
               )
  unname(mapping[user]) %||% "Kevin"
}

who_initials <- c("kcarriere" = "K",
                  "pjones" = "P", "bjones" = "B",
                  "rhammond" = "R", "ehammond" = "E",
                  "slafontaine" = "S", "alafontaine" = "A",
                  "wburdette" = "W", "amcfeeters" = "A",
                  "nburdette" = "N", "tmcfeeters" = "T",
                  "mfasano" = "M", "afasano" = "M",
                  "vvega" = "V")


win_targets <- list(
  admin = c(supporters = 3300, doors = 4500),
  ns = c(supporters = 3300, doors = 4500),
  sb = c(supporters = 2000, doors = 3000),
  d17 = c(supporters = 6000, doors = 8000),
  paw = c(supporters = 750, doors = 1000),
  d49 = c(supporters = 3300, doors = 4500)
)

scenario_access <- list(
  admin = c("NSDem", "NSInd", "NSDandI", "NSAll", "D17Dem", "D17Ind", "D17DandI", "Pawtucket",
            "D49Dem", "D49Ind", "D49DandI", "KevinOnly", "KevinOrDonor"),
  ns    = c("NSDem", "NSInd", "NSDandI", "NSAll", "KevinOnly", "KevinOrDonor"),
  sb    = c("NSDem", "NSInd", "NSDandI", "NSAll"),
  d17   = c("D17Dem", "D17Ind", "D17DandI"),
  d49   = c("D49Dem", "D49Ind", "D49DandI"),
  paw = c("Pawtucket")
)

address_trim <- list(
  NSDem   = ", North Smithfield, RI, 02896",
  NSInd   = ", North Smithfield, RI, 02896",
  NSDandI = ", North Smithfield, RI, 02896"
)

# Town suffixes to strip from full addresses when displaying. Keyed by
# the substring that appears in the address — order matters, longer
# patterns first so we don't half-match.
trim_display_address <- c(
  ", North Smithfield, RI, 02896",
  ", Pawtucket, RI, 02860",
  ", Pawtucket, RI, 02861",
  ", Pawtucket, RI, 02862",
  ", Pawtucket, RI, 02863",
  ", Pawtucket, RI"  # fallback for any missing zips
)
