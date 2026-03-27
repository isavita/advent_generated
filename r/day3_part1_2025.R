
lines <- readLines("input.txt", warn = FALSE)

calc <- function(s) {
  chars <- strsplit(s, "")[[1]]
  n <- length(chars)
  if (n < 2) return(0)
  
  for (d in 9:0) {
    first_occ <- match(as.character(d), chars)
    if (!is.na(first_occ) && first_occ < n) {
      suffix <- chars[(first_occ + 1):n]
      digits <- as.integer(suffix[suffix >= "0" & suffix <= "9"])
      if (length(digits) > 0) {
        return(d * 10 + max(digits))
      }
    }
  }
  return(0)
}

total <- sum(vapply(lines, calc, FUN.VALUE = numeric(1)))
cat(total, "\n")
