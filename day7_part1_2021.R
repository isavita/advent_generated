
data <- as.numeric(unlist(strsplit(readLines("input.txt"), ",")))
data <- sort(data)

min_fuel <- 2^31 - 1
for (i in min(data):max(data)) {
  fuel <- 0
  for (pos in data) {
    fuel <- fuel + abs(pos - i)
  }
  if (fuel < min_fuel) {
    min_fuel <- fuel
  }
}
cat(min_fuel, "\n")
