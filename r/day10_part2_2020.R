
data <- scan("input.txt", quiet = TRUE)
adapters <- sort(c(0, data))
adapters <- c(adapters, max(adapters) + 3)
n <- length(adapters)
ways <- numeric(n)
ways[1] <- 1

for (i in 2:n) {
  for (j in (i - 1):max(1, i - 3)) {
    if (adapters[i] - adapters[j] <= 3) {
      ways[i] <- ways[i] + ways[j]
    }
  }
}

cat(format(ways[n], scientific = FALSE), "\n")
