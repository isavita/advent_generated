
library(digest)

salt <- readLines("input.txt", warn = FALSE)[1]
cache <- new.env(hash = TRUE, parent = emptyenv())

get_hash <- function(n) {
  key <- as.character(n)
  if (!exists(key, envir = cache)) {
    assign(key, digest(paste0(salt, n), algo = "md5", serialize = FALSE), envir = cache)
  }
  get(key, envir = cache)
}

found_keys <- 0
index <- 0

while (found_keys < 64) {
  h <- get_hash(index)
  m <- regexpr("(.)\\1\\1", h, perl = TRUE)
  
  if (m != -1) {
    char <- substr(h, m, m)
    target <- strrep(char, 5)
    for (j in (index + 1):(index + 1000)) {
      if (grepl(target, get_hash(j), fixed = TRUE)) {
        found_keys <- found_keys + 1
        break
      }
    }
  }
  index <- index + 1
}

cat(index - 1, "\n")
