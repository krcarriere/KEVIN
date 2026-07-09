build_direction_links <- function(addresses, max_stops) {
  n <- length(addresses)
  if (n < 2) return(list())
  links <- list()
  i <- 1
  chunk_num <- 1
  while (i <= n) {
    end_i <- min(i + max_stops - 1, n)
    if (end_i == i && chunk_num > 1) break
    chunk <- addresses[i:end_i]
    origin      <- utils::URLencode(chunk[1], reserved = TRUE)
    destination <- utils::URLencode(chunk[length(chunk)], reserved = TRUE)
    if (length(chunk) > 2) {
      waypoints <- paste(
        sapply(chunk[2:(length(chunk) - 1)], function(x) utils::URLencode(x, reserved = TRUE)),
        collapse = "%7C"
      )
      url <- paste0("https://www.google.com/maps/dir/?api=1",
                    "&origin=", origin, "&destination=", destination,
                    "&travelmode=walking", "&waypoints=", waypoints)
    } else {
      url <- paste0("https://www.google.com/maps/dir/?api=1",
                    "&origin=", origin, "&destination=", destination,
                    "&travelmode=walking")
    }
    links[[chunk_num]] <- list(label = paste0("Directions: Stop ", i, " to Stop ", end_i), url = url)
    chunk_num <- chunk_num + 1
    i <- end_i + 1
  }
  links
}