
# Read input from file
input <- as.matrix(read.table("input.txt"))

# Part 1
result_part1 <- sum(apply(input, 1, function(x) max(x) - min(x)))

# Part 2
result_part2 <- sum(apply(input, 1, function(x) {
  for (i in 1:length(x)) {
    for (j in 1:length(x)) {
      if (i != j && x[i] %% x[j] == 0) {
        return(x[i] / x[j])
      }
    }
  }
}))

print(result_part1)
print(result_part2)
