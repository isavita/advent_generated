
calculate_ways_to_win <- function(time, record) {
  t <- 1:(time-1)
  sum((time - t) * t > record)
}

input <- readLines("input.txt")
times <- as.numeric(strsplit(input[1], "\\s+")[[1]][-1])
distances <- as.numeric(strsplit(input[2], "\\s+")[[1]][-1])

total_ways <- prod(mapply(calculate_ways_to_win, times, distances))
cat(total_ways, "\n")
