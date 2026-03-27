library(openssl)

p <- trimws(readLines("input.txt", warn = FALSE)[1])
q <- list(list(x = 0, y = 0, s = ""))
head <- 1
dirs <- c("U", "D", "L", "R")
dx <- c(0, 0, -1, 1)
dy <- c(-1, 1, 0, 0)

while (head <= length(q)) {
  curr <- q[[head]]
  head <- head + 1
  if (curr$x == 3 && curr$y == 3) {
    cat(curr$s, "\n")
    break
  }
  h <- as.character(md5(paste0(p, curr$s)))
  for (i in 1:4) {
    ch <- substr(h, i, i)
    if (ch >= "b" && ch <= "f") {
      nx <- curr$x + dx[i]
      ny <- curr$y + dy[i]
      if (nx >= 0 && nx < 4 && ny >= 0 && ny < 4) {
        q[[length(q) + 1]] <- list(x = nx, y = ny, s = paste0(curr$s, dirs[i]))
      }
    }
  }
}