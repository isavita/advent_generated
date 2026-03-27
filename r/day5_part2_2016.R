
library(openssl)
options(scipen = 999)

d <- trimws(readLines("input.txt", warn = FALSE)[1])
a <- rep(NA, 8)
i <- 0
batch_size <- 5000

while (any(is.na(a))) {
  h <- md5(paste0(d, i:(i + batch_size - 1)))
  m <- which(substr(h, 1, 5) == "00000")
  for (j in m) {
    c6 <- substr(h[j], 6, 6)
    if (c6 %in% as.character(0:7)) {
      p <- as.integer(c6) + 1
      if (is.na(a[p])) {
        a[p] <- substr(h[j], 7, 7)
        if (!any(is.na(a))) break
      }
    }
  }
  i <- i + batch_size
}

cat(paste0(a, collapse = ""), "\n")
