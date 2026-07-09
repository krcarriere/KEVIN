# Removed 5/19/26 - Not needed based on above remove. 
#  output$map_embed <- renderUI({
#   scen <- input$scenario
#   prec <- input$precinct
#   
#   if (is.null(scen) || scen == "" || is.null(prec) || prec == "") {
#     return(tags$p(class = "text-muted", "Select a voter base and precinct to see the map."))
#   }
#   
#   map_urls <- list(
#     "NSDem_2501"  = "https://www.google.com/maps/d/u/0/embed?mid=1CD7uk5f-Xb5EVU5hhrS6yBnYFMKrTm8&ehbc=2E312F",
#     "NSDem_2502"  = "https://www.google.com/maps/d/u/0/embed?mid=19oYWeWDkWC2mQGlom5zj9xCKPMKB_cE&ehbc=2E312F",
#     "NSDem_2503"  = "https://www.google.com/maps/d/u/0/embed?mid=1A2wVRP9nqsXsF51jzvlFkLQAvj2BZJw&ehbc=2E312F",
#     "NSDem_2504"  = "https://www.google.com/maps/d/u/0/embed?mid=1fcK81cjeuYn9AE6QZmGFrr2yM8ZKxzU&ehbc=2E312F",
#     "NSDem_2505"  = "https://www.google.com/maps/d/u/0/embed?mid=1nPmrIT3rFmZHKJBWBknPf7wwRv4Em4s&ehbc=2E312F",
#     
#     "NSInd_2501" = "https://www.google.com/maps/d/u/0/embed?mid=1h-jF_07wXWPrvj8i_T_s0I3D36PpIhI&ehbc=2E312F",
#     "NSInd_2502" = "https://www.google.com/maps/d/u/0/embed?mid=1j-3DH2nWgKAG6Uh_tn84lAh5XKzEqNU&ehbc=2E312F",
#     "NSInd_2503" = "https://www.google.com/maps/d/u/0/embed?mid=1g_ODougpdZwsbf5EU8oivPi_MNi01VI&ehbc=2E312F",
#     "NSInd_2504" = "https://www.google.com/maps/d/u/0/embed?mid=1h-jF_07wXWPrvj8i_T_s0I3D36PpIhI&ehbc=2E312F",
#     "NSInd_2505" = "https://www.google.com/maps/d/u/0/embed?mid=1ahHEwpcGI4Dl-XF9YTUkVhYnrDRP--w&ehbc=2E312F",
#     
#     "NSDandI_2501" = "https://www.google.com/maps/d/u/0/embed?mid=10gIYIFLuLBiTEOyPMK7Peg4wH9ZTMzg&ehbc=2E312F",
#     "NSDandI_2502" = "https://www.google.com/maps/d/u/0/embed?mid=1XDzFGNoaCqxCQVRueplWmU2UnCActYg&ehbc=2E312F",
#     "NSDandI_2503" = "https://www.google.com/maps/d/u/0/embed?mid=1zgIhJzOcRVfSfoytThQvILocnLh0l5A&ehbc=2E312F",
#     "NSDandI_2504" = "https://www.google.com/maps/d/u/0/embed?mid=1386mrxhu4jTzyyJtqbJJufsifVrWQsM&ehbc=2E312F",
#     "NSDandI_2505" = "https://www.google.com/maps/d/u/0/embed?mid=1B62AfIWBn5lZSZeFTU1xAvJmN2qrRYY&ehbc=2E312F",
#     
#     "D17Dem_1702" = "https://www.google.com/maps/d/u/0/embed?mid=12LehN935nftkodxpx8TWwAr-dScVsMk&ehbc=2E312F",
#     "D17Dem_1703" = "https://www.google.com/maps/d/u/0/embed?mid=1zxY04S6_peeGCqu0uIdczvAgao9W_xs&ehbc=2E312F",
#     "D17Dem_1704" = "https://www.google.com/maps/d/u/0/embed?mid=1OFgLE5XuqW-a5MN4kHz6grbytzBkxpg&ehbc=2E312F",
#     "D17Dem_1705" = "https://www.google.com/maps/d/u/0/embed?mid=1jPsRLMs-xToGE_KEIF7wbEFCP_VS6iQ&ehbc=2E312F",
#     "D17Dem_1706" = "https://www.google.com/maps/d/u/0/embed?mid=1kP-tvDRya00KY5lvh6MC8cH9p1VPpGk&ehbc=2E312F",
#     "D17Dem_1707" = "https://www.google.com/maps/d/u/0/embed?mid=1FONrEsVHk2niEU2xuaN_kY0idMkOJ_c&ehbc=2E312F",
#     "D17Dem_1708" = "https://www.google.com/maps/d/u/0/embed?mid=1DU-ViwJhi7-DfrAoMx1otaWjOVlrdpM&ehbc=2E312F", 
#     "D17Dem_1709" = "https://www.google.com/maps/d/u/0/embed?mid=1dsLnhYWp5LAOmBpx1W4id4Xr5AfmnNA&ehbc=2E312F",
#     "D17Dem_2501" = "https://www.google.com/maps/d/u/0/embed?mid=1SGva7mSFi76r5BMKjTEFFMgX_0M9DW8&ehbc=2E312F",
#     "D17Dem_2502" = "https://www.google.com/maps/d/u/0/embed?mid=1SAzyjpL4ID-rEnfg2AgwXA0DDOeQh_Y&ehbc=2E312F",
#     "D17Dem_2409" = "https://www.google.com/maps/d/u/0/embed?mid=1EO4lQSNcQATUR5BuKskkqkauuIjaslA&ehbc=2E312F",
#     
#     "D17Ind_1702" = "https://www.google.com/maps/d/u/0/embed?mid=1UIpOxlZKVWfv4Yxh20YE1Gs_y1kRjEs&ehbc=2E312F",
#     "D17Ind_1703" = "https://www.google.com/maps/d/u/0/embed?mid=1CU6VB2hYMnAhjeKw-z4CPDpdQlKmrUo&ehbc=2E312F",
#     "D17Ind_1704" = "https://www.google.com/maps/d/u/0/embed?mid=1c4GcZ-LHXrOHkrPX9vCBvJSKtRQ_nQg&ehbc=2E312F",
#     "D17Ind_1705" = "https://www.google.com/maps/d/u/0/embed?mid=1gtZ_Yx4OuJIxvDH-I0w6CZxs9M3iPiA&ehbc=2E312F",
#     "D17Ind_1706" = "https://www.google.com/maps/d/u/0/embed?mid=1gZRbwc2p1zM0vWP7O5m95PKLAQXg6hA&ehbc=2E312F",
#     "D17Ind_1707" = "https://www.google.com/maps/d/u/0/embed?mid=1gRsSo9qbDILhZhCGPNmyE44Daew8iCo&ehbc=2E312F",
#     "D17Ind_1708" = "https://www.google.com/maps/d/u/0/embed?mid=1Eg-OY-h1pLDtgmIJCPq0vTXP5P8ugD0&ehbc=2E312F",
#     "D17Ind_1709" = "https://www.google.com/maps/d/u/0/embed?mid=16tP8Z_wnxTu0rNfEhnHn82kuwz2x50I&ehbc=2E312F",
#     "D17Ind_2409" = "https://www.google.com/maps/d/u/0/embed?mid=13TsLMEmWjJ9ZG95nzSf9qxZwbe-VeH8&ehbc=2E312F",
#     "D17Ind_2501" = "https://www.google.com/maps/d/u/0/embed?mid=1_uAYuxlruMjq5_VMjjl0QMlYcaRNs2I&ehbc=2E312F",
#     "D17Ind_2502" = "https://www.google.com/maps/d/u/0/embed?mid=1AOlBkiwxEe5dahCmS1_TcTCu9Muxz4o&ehbc=2E312F",
#     
#     "D17DandI_1702" = "https://www.google.com/maps/d/u/0/embed?mid=1gQaskR5Pasu9bkBtX1c2OsM1OC8Nx48&ehbc=2E312F",
#     "D17DandI_1703" = "https://www.google.com/maps/d/u/0/embed?mid=1Mfjp6Nr-x_T_E2rqtsO1b0CGxx2sH4g&ehbc=2E312F",
#     "D17DandI_1704" = "https://www.google.com/maps/d/u/0/embed?mid=1omqu6mwAriJz-bjmGtXduAtoIRZKW1s&ehbc=2E312F",
#     "D17DandI_1705" = "https://www.google.com/maps/d/u/0/embed?mid=1YDByz1M5HzNb44Ib7XVQ85qcF_GJM7I&ehbc=2E312F",
#     "D17DandI_1706" = "https://www.google.com/maps/d/u/0/embed?mid=1m0P0dCTSbFwYECvswDGVT8frzryol8I&ehbc=2E312F",
#     "D17DandI_1707" = "https://www.google.com/maps/d/u/0/embed?mid=1KZXOL41QcPFFPjmMAP5hEGn3B0tFsfo&ehbc=2E312F",
#     "D17DandI_1708" = "https://www.google.com/maps/d/u/0/embed?mid=1FDo-wOiUR8Lbi9DklNNwpOTXajXJdjs&ehbc=2E312F",
#     "D17DandI_1709" = "https://www.google.com/maps/d/u/0/embed?mid=1FaSbUkUJ7IGePy7pVnWn9y0Xn8uHZFM&ehbc=2E312F",
#     "D17DandI_2409" = "https://www.google.com/maps/d/u/0/embed?mid=1UXvemmFBHcJ-4wZKExhjhgbgztLA1TQ&ehbc=2E312F",
#     "D17DandI_2501" = "https://www.google.com/maps/d/u/0/embed?mid=1uuAVK_KyZUq3yl5Z-1xwMRKUR4UCdaM&ehbc=2E312F",
#     "D17DandI_2502" = "https://www.google.com/maps/d/u/0/embed?mid=1F2tuwkK16o7FEHLT8H4ev5kjw5gytJM&ehbc=2E312F",
#     
#     
#     "Paw_2601" = "https://www.google.com/maps/d/u/0/embed?mid=1fzcGbVtbIcrBqm46OmXZj3emesMglYY&ehbc=2E312F",
#     "Paw_2602" = "https://www.google.com/maps/d/u/0/embed?mid=1oiw1OdPQ2ufb-5f3OQVgAk_cgdvBKZA&ehbc=2E312F",
#     "Paw_2603" = "https://www.google.com/maps/d/u/0/embed?mid=1CrShns3tqwiEYM6X1ZFDLVCH5Ch3aSk&ehbc=2E312F"
#   )
#   
#   key <- paste0(scen, "_", prec)
#   url <- map_urls[[key]]
#   
#   if (is.null(url)) return(tags$p("No map available for this precinct yet."))
#   
#   tags$iframe(
#     src = url,
#     width = "100%",
#     height = "600px",
#     style = "border: none; border-radius: 4px;"
#   )
# })

# concernsResultsServer("concerns_mod",
#                       con               = con,
#                       searchable_voters = searchable_voters,
#                       credentials       = credentials,
#                       data_refresh      = data_refresh
# )
