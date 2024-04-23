file <- "input.txt"
seatIDs <- c()

# Define the functions
decode <- function(pass) {
  row <- binaryToInt(substr(pass, 1, 7))
  column <- binaryToInt(substr(pass, 8, 10))
  return(row * 8 + column)
}

binaryToInt <- function(binaryStr) {
  result <- 0
  for (i in 1:nchar(binaryStr)) {
    if (substr(binaryStr, i, i) == "1") {
      result <- result + 2^(nchar(binaryStr) - i)
    }
  }
  return(result)
}

# Open the file
con <- file("input.txt", "r")

# Read the file line by line
while (TRUE) {
  line <- readLines(con, n = 1)
  if (length(line) == 0) break
  pass <- gsub("F", "0", line)
  pass <- gsub("B", "1", pass)
  pass <- gsub("L", "0", pass)
  pass <- gsub("R", "1", pass)
  seatID <- decode(pass)
  seatIDs <- c(seatIDs, seatID)
}

# Close the file
close(con)

# Sort the seat IDs
seatIDs <- sort(seatIDs)

# Find the missing seat ID
for (i in 1:(length(seatIDs) - 1)) {
  if (seatIDs[i + 1] != seatIDs[i] + 1) {
    print(seatIDs[i] + 1)
    break
  }
}