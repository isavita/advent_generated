

lines <- readLines("input.txt")
numbers <- lapply(strsplit(lines, ","), as.numeric)
positions <- unlist(numbers)

positions <- sort(positions)

calculateNewFuel <- function(currentPosition, newPosition) {
  diff <- abs(currentPosition - newPosition)
  return ((diff * (diff + 1)) / 2)
}

abs <- function(n) {
  if (n < 0) {
    return (-n)
  }
  return (n)
}

min_fuel <- 2^31 - 1
for (i in positions[1]:positions[length(positions)]) {
  fuel <- 0
  for (pos in positions) {
    fuel <- fuel + calculateNewFuel(pos, i)
  }
  if (fuel < min_fuel) {
    min_fuel <- fuel
  }
}
cat(min_fuel, "\n")

