
pts <- read.table("input.txt", sep = ",", header = FALSE, fill = TRUE)
pts <- pts[complete.cases(pts), , drop = FALSE]
n <- nrow(pts)

if (n < 2) {
  cat("Not enough points to form circuits.\n")
} else {
  m <- as.matrix(dist(pts)^2)
  m[upper.tri(m, diag = TRUE)] <- Inf
  
  num_edges <- n * (n - 1) / 2
  idx <- order(m)[1:min(num_edges, 1000)]
  
  rows <- (idx - 1) %% n + 1
  cols <- (idx - 1) %/% n + 1
  
  p <- 1:n
  sz <- rep(1, n)
  
  for (k in seq_along(idx)) {
    u <- rows[k]
    v <- cols[k]
    
    while (p[u] != u) {
      p[u] <- p[p[u]]
      u <- p[u]
    }
    while (p[v] != v) {
      p[v] <- p[p[v]]
      v <- p[v]
    }
    
    if (u != v) {
      if (sz[u] < sz[v]) {
        p[u] <- v
        sz[v] <- sz[v] + sz[u]
      } else {
        p[v] <- u
        sz[u] <- sz[u] + sz[v]
      }
    }
  }
  
  comp_sizes <- sort(sz[p == 1:n], decreasing = TRUE)
  ans <- prod(comp_sizes[1:min(3, length(comp_sizes))])
  
  cat(sprintf("Product of three largest circuit sizes: %.0f\n", ans))
}
