
data <- readLines("input.txt")
totalDiff <- 0

calculateMemoryLength <- function(s) {
  length <- 0
  inEscape <- FALSE
  hexCount <- 0
  
  for (i in 2:(nchar(s) - 1)) {
    if (hexCount > 0) {
      hexCount <- hexCount - 1
    } else if (inEscape) {
      if (substr(s, i, i) == "x") {
        hexCount <- 2
      }
      inEscape <- FALSE
      length <- length + 1
    } else if (substr(s, i, i) == "\\") {
      inEscape <- TRUE
    } else {
      length <- length + 1
    }
  }
  
  return(length)
}

for (line in data) {
  codeLength <- nchar(line)
  memoryLength <- calculateMemoryLength(line)
  totalDiff <- totalDiff + codeLength - memoryLength
}

cat(totalDiff, "\n")
