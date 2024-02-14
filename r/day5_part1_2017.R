
data <- readLines("input.txt")
offsets <- as.integer(data)

index <- 1
steps <- 0

while(index >= 1 && index <= length(offsets)) {
  jump <- offsets[index]
  offsets[index] <- offsets[index] + 1
  index <- index + jump
  steps <- steps + 1
}

cat(steps, "\n")
