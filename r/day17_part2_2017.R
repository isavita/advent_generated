
data <- as.numeric(readLines("input.txt"))
steps <- data[1]
current_pos <- 0
value_after_zero <- 0

for (i in 1:50000000) {
  current_pos <- (current_pos + steps) %% i
  if (current_pos == 0) {
    value_after_zero <- i
  }
  current_pos <- current_pos + 1
}

cat(value_after_zero, "\n")
