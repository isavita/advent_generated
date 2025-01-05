
input <- readLines("input.txt")
grid <- do.call(rbind, lapply(input, function(x) as.integer(strsplit(x, "")[[1]])))
nr <- nrow(grid)
nc <- ncol(grid)
dp <- matrix(-1, nrow = nr, ncol = nc)
dirs <- list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))

dfs <- function(r, c) {
  if (dp[r, c] != -1) {
    return(dp[r, c])
  }
  h <- grid[r, c]
  if (h == 9) {
    dp[r, c] <<- 1
    return(1)
  }
  sum <- 0
  for (d in dirs) {
    nr2 <- r + d[1]
    nc2 <- c + d[2]
    if (nr2 < 1 || nr2 > nr || nc2 < 1 || nc2 > nc) {
      next
    }
    if (grid[nr2, nc2] == h + 1) {
      sum <- sum + dfs(nr2, nc2)
    }
  }
  dp[r, c] <<- sum
  return(sum)
}

total <- 0
for (r in 1:nr) {
  for (c in 1:nc) {
    if (grid[r, c] == 0) {
      total <- total + dfs(r, c)
    }
  }
}
cat(total, "\n")
