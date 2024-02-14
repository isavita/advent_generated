
discs <- readLines("input.txt")
total_positions <- as.integer(sub(".*has (\\d+) positions.*", "\\1", discs))
start_position <- as.integer(sub(".*position (\\d+).*", "\\1", discs))

time <- 0
while (TRUE) {
  if (sum((start_position + time + seq_along(total_positions)) %% total_positions) == 0) {
    cat(time, "\n")
    break
  }
  time <- time + 1
}
