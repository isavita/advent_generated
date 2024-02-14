
sum <- 0
lines <- readLines("input.txt")

for (line in lines) {
  if (line == "") {
    next
  }
  
  firstDigit <- -1
  lastDigit <- -1
  
  for (r in strsplit(line, "")[[1]]) {
    if (grepl("[0-9]", r)) {
      if (firstDigit == -1) {
        firstDigit <- as.numeric(r)
      }
      lastDigit <- as.numeric(r)
    }
  }
  
  if (firstDigit != -1 & lastDigit != -1) {
    value <- as.numeric(paste(firstDigit, lastDigit, sep = ""))
    sum <- sum + value
  }
}

cat(sum, "\n")
