
options(scipen = 999)
lines <- readLines("input.txt", warn = FALSE)
ans <- 0

for (line in lines) {
  if (line == "") next
  p <- strsplit(line, " ")[[1]]
  s <- paste(rep(p[1], 5), collapse = "?")
  g <- rep(as.numeric(strsplit(p[2], ",")[[1]]), 5)
  
  n <- nchar(s)
  m <- length(g)
  chars <- strsplit(s, "")[[1]]
  memo <- array(-1, dim = c(n, m + 1, max(g) + 1))
  
  f <- function(i, j, k) {
    if (i > n) return(if ((j > m && k == 0) || (j == m && k == g[m])) 1 else 0)
    if (memo[i, j, k + 1] != -1) return(memo[i, j, k + 1])
    
    res <- 0
    cur <- chars[i]
    if (cur != "#") {
      if (k == 0) {
        res <- res + f(i + 1, j, 0)
      } else if (j <= m && k == g[j]) {
        res <- res + f(i + 1, j + 1, 0)
      }
    }
    if (cur != ".") {
      if (j <= m && k < g[j]) {
        res <- res + f(i + 1, j, k + 1)
      }
    }
    
    memo[i, j, k + 1] <<- res
    return(res)
  }
  
  ans <- ans + f(1, 1, 0)
}

cat(format(ans, scientific = FALSE), "\n")
