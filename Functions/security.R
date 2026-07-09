# ══════════════════════════════════════════════════════════════════════════
# security.R
#
# Small helpers for hardening KEVIN against common web-app vulnerabilities.
# Keep this file narrow: one concept per function, well-commented, so
# future changes are obvious and auditable.

sanitize_user_input <- function(x) {
  if (is.null(x)) return(x)
  
  x_char <- as.character(x)
  
  x_char <- trimws(x)
  x_char <- gsub("[\x01-\x08\x0B\x0C\x0E-\x1F\x7F]", "", x_char, perl = TRUE)
  # I had this at one point, but have gotten rid of it in favor of the gsub.
  #x_char <- gsub("[[:cntrl:]]", "", x_char)
  
  x_char <- gsub("[\t\r]", " ", x_char)
  x_char <- htmltools::htmlEscape(x_char)
  
  # Preserve NAs from original
  x_char[is.na(x)] <- NA_character_
  x_char
}