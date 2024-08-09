calculate_signal_strengths <- function(filename) {
  instructions <- readLines(filename)
  
  X <- 1
  cycle <- 0
  signal_strengths <- numeric(0)
  interesting_cycles <- seq(20, 220, by=40)
  
  for (instruction in instructions) {
    if (instruction == "noop") {
      cycle <- cycle + 1
      if (cycle %in% interesting_cycles) {
        signal_strengths <- c(signal_strengths, cycle * X)
      }
    } else {
      for (i in 1:2) {
        cycle <- cycle + 1
        if (cycle %in% interesting_cycles) {
          signal_strengths <- c(signal_strengths, cycle * X)
        }
      }
      X <- X + as.numeric(sub("addx ", "", instruction))
    }
  }
  
  sum(signal_strengths)
}

result <- calculate_signal_strengths("input.txt")
cat(result, "\n")