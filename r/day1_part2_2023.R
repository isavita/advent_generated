
lines <- readLines("input.txt")
digits <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
sum <- 0

for (line in lines) {
  first_digit <- NA
  last_digit <- NA
  
  for (i in 1:nchar(line)) {
    char <- substr(line, i, i)
    if (grepl("[0-9]", char)) {
      digit <- as.integer(char)
      if (is.na(first_digit)) {
        first_digit <- digit
      }
      last_digit <- digit
    } else {
      for (j in 1:length(digits)) {
        if (startsWith(substr(line, i, nchar(line)), digits[j])) {
          if (is.na(first_digit)) {
            first_digit <- j - 1
          }
          last_digit <- j - 1
          break
        }
      }
    }
  }
  sum <- sum + 10 * first_digit + last_digit
}

cat(sum, "\n")
