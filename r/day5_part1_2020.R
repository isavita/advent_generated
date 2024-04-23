file <- "input.txt"
maxSeatID <- 0

# Define the functions before using them
decode <- function(pass) {
  row <- binaryToInt(substring(pass, 1, 7))
  column <- binaryToInt(substring(pass, 8, 10))
  return(row*8 + column)
}

binaryToInt <- function(binaryStr) {
  result <- 0
  for (i in 1:nchar(binaryStr)) {
    if (substr(binaryStr, i, i) == "1") {
      result <- bitwOr(result, 2^(nchar(binaryStr) - i))
    }
  }
  return(result)
}

# Now read the file and process the lines
for (line in readLines(file)) {
  pass <- gsub("F", "0", line)
  pass <- gsub("B", "1", pass)
  pass <- gsub("L", "0", pass)
  pass <- gsub("R", "1", pass)
  seatID <- decode(pass)
  if (seatID > maxSeatID) {
    maxSeatID <- seatID
  }
}

print(maxSeatID)