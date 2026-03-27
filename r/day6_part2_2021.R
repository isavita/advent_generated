
data <- scan("input.txt", sep = ",", quiet = TRUE)
counts <- as.numeric(tabulate(data + 1, nbins = 9))

for (day in 1:256) {
  new_fish <- counts[1]
  counts <- c(counts[2:9], new_fish)
  counts[7] <- counts[7] + new_fish
}

cat(format(sum(counts), scientific = FALSE), "\n")
