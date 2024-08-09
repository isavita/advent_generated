data <- as.integer(trimws(readLines("input.txt")))
steps <- data
buffer <- c(0)
currentPos <- 1

for (i in 1:2017) {
  currentPos <- (currentPos + steps) %% length(buffer)
  buffer <- append(buffer, 0)
  buffer[(currentPos + 2):length(buffer)] <- buffer[(currentPos + 1):(length(buffer) - 1)]
  buffer[currentPos + 1] <- i
  currentPos <- currentPos + 1
}

next_value <- buffer[(which(buffer == 2017) + 1) %% length(buffer)]
print(next_value)