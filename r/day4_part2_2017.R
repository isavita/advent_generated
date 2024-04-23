sort_string <- function(w) {
  s <- strsplit(w, "")[[1]]
  s <- s[order(s)]
  return(paste(s, collapse = ""))
}

passphrases <- readLines("input.txt")
valid_count <- 0

for (passphrase in passphrases) {
  words <- strsplit(passphrase, "\\s+")[[1]]
  word_set <- list()
  
  valid <- TRUE
  for (word in words) {
    sorted_word <- sort_string(word)
    if (isTRUE(any(sapply(word_set, function(x) identical(x, sorted_word))))) {
      valid <- FALSE
      break
    }
    word_set <- c(word_set, list(sorted_word))
  }
  
  if (valid) {
    valid_count <- valid_count + 1
  }
}

print(valid_count)