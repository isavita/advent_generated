
input <- readLines("input.txt")
earliestDeparture <- as.numeric(input[1])
busIDs <- strsplit(input[2], ",")[[1]]

earliestBusID <- 0
minWaitTime <- earliestDeparture

for (id in busIDs) {
  if (id == "x") {
    next
  }
  busID <- as.numeric(id)
  waitTime <- busID - (earliestDeparture %% busID)
  if (waitTime < minWaitTime) {
    minWaitTime <- waitTime
    earliestBusID <- busID
  }
}

cat(earliestBusID * minWaitTime, "\n")
