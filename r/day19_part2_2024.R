
options(scipen = 999)
input <- readLines("input.txt", warn = FALSE)
if (length(input) > 0) {
  patterns <- trimws(strsplit(input[1], ",")[[1]])
  pat_lens <- nchar(patterns)
  designs <- input[nchar(input) > 0][-1]

  total_ways <- 0
  for (d in designs) {
    n <- nchar(d)
    dp <- numeric(n + 1)
    dp[1] <- 1
    for (i in 1:n) {
      if (dp[i] > 0) {
        sub_d <- substr(d, i, n)
        matches <- which(startsWith(sub_d, patterns))
        for (idx in matches) {
          nxt <- i + pat_lens[idx]
          dp[nxt] <- dp[nxt] + dp[i]
        }
      }
    }
    total_ways <- total_ways + dp[n + 1]
  }
  cat(total_ways, "\n")
}
