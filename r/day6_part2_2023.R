calculateWaysToWinLongRace <- function(time, record) {
  waysToWin <- 0
  for (holdTime in 1:(time - 1)) {
    travelTime <- time - holdTime
    distance <- holdTime * travelTime
    if (distance > record) {
      waysToWin <- waysToWin + 1
    }
  }
  return(waysToWin)
}

input <- readLines("input.txt")
time <- 0
distance <- 0

for (line in input) {
  if (nchar(line) == 0) next
  parts <- strsplit(line, ":")[[1]]
  value <- as.numeric(gsub(" ", "", parts[2]))
  if (time == 0) {
    time <- value
  } else {
    distance <- value
  }
}

waysToWin <- calculateWaysToWinLongRace(time, distance)
print(waysToWin)