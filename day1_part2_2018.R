
# Read input from file
input <- scan("input.txt", what = numeric())

# Part 1
result_part1 <- sum(input)

# Part 2
freq <- 0
freq_seen <- c()
found <- FALSE
while(!found) {
  for (i in input) {
    freq <- freq + i
    if (freq %in% freq_seen) {
      result_part2 <- freq
      found <- TRUE
      break
    } else {
      freq_seen <- c(freq_seen, freq)
    }
  }
}

print(result_part1)
print(result_part2)
