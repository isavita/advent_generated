
library(openssl)

k <- trimws(readLines("input.txt", n = 1, warn = FALSE), "right")
n <- 0
repeat {
  h <- md5(paste0(k, n + 0:49999))
  m <- which(startsWith(h, "00000"))
  if (length(m)) {
    cat(n + m[1] - 1, "\n")
    break
  }
  n <- n + 50000
}
