dated_col <- function(prefix) {
  paste0(prefix, "_", format(Sys.Date(), "%m%d%y"))
}

# Campaign-specific stopwords — tune these as you see what comes through
custom_stopwords <- c(
  "town", "think", "feel", "people", "need", "want", "get",
  "just", "really", "one", "also", "going", "know", "like",
  "north", "smithfield"
)

# Phrases to preserve as single tokens. Replace before splitting.
preserve_phrases <- c(
  "property tax", "police station", "cost of living",
  "town council", "school committee", "water and sewer",
  "code enforcement", "tax base"
)

tokenize_other_text <- function(text_vec) {
  if (length(text_vec) == 0) return(tibble::tibble(word = character(0), n = integer(0)))
  
  txt <- tolower(paste(text_vec, collapse = " "))
  
  # Preserve known phrases as single tokens
  for (phrase in preserve_phrases) {
    txt <- gsub(phrase, gsub(" ", "_", phrase), txt, fixed = TRUE)
  }
  
  # Strip punctuation, keep underscores + word chars
  txt <- gsub("[^a-z0-9_\\s]", " ", txt, perl = TRUE)
  
  # Split, filter, stem
  words <- strsplit(txt, "\\s+")[[1]]
  words <- words[nchar(words) >= 3]
  words <- words[!words %in% tm::stopwords("en")]
  words <- words[!words %in% custom_stopwords]
  #words <- SnowballC::wordStem(words, language = "en")
  
  # Restore phrase readability — replace underscores with spaces for display
  words <- gsub("_", " ", words)
  
  tibble::tibble(word = words) |>
    dplyr::count(word, sort = TRUE) #|>
    #dplyr::filter(n >= 2)  # drop one-offs
}

# Strip recognized town/state/zip suffixes from an address for display.
# Returns the original address if no pattern matches (so addresses outside
# expected jurisdictions still show, just unstripped).
display_address <- function(addresses) {
  out <- addresses
  for (pattern in trim_display_address) {
    out <- stringr::str_remove(out, stringr::fixed(pattern))
  }
  out
}