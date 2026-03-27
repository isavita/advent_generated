
v <- scan("input.txt", quiet = TRUE)
n <- length(v)
v <- v * 811589153
idx <- 1:n

for (r in 1:10) {
  for (i in 1:n) {
    p <- match(i, idx)
    new_p <- (p - 1 + v[i]) %% (n - 1)
    idx <- append(idx[-p], i, after = new_p)
  }
}

res <- v[idx]
z <- match(0, res)
ans <- sum(res[(z + c(1000, 2000, 3000) - 1) %% n + 1])
cat(sprintf("%.0f\n", ans))
