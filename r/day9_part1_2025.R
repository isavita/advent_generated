
df <- read.table("input.txt", sep = ",")
x <- df$V1
y <- df$V2
n <- length(x)
ans <- 0
for (i in seq_len(n)) {
  ans <- max(ans, (abs(x[i] - x[i:n]) + 1) * (abs(y[i] - y[i:n]) + 1))
}
cat(sprintf("%.0f\n", ans))
