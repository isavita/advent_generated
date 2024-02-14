
data <- as.numeric(readLines("input.txt"))
target <- data[1]

sideLength <- ceiling(sqrt(target))
if (sideLength %% 2 == 0) {
  sideLength <- sideLength + 1
}

maxValue <- sideLength * sideLength
stepsFromEdge <- (sideLength - 1) / 2
distanceToMiddle <- 0

for (i in 0:3) {
  middlePoint <- maxValue - stepsFromEdge - (sideLength - 1) * i
  distance <- abs(target - middlePoint)
  if (distance < distanceToMiddle || i == 0) {
    distanceToMiddle <- distance
  }
}

manhattanDistance <- stepsFromEdge + distanceToMiddle

cat(manhattanDistance, "\n")
