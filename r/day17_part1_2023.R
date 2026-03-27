
grid <- do.call(rbind, lapply(strsplit(readLines("input.txt"), ""), as.integer))
nr <- nrow(grid); nc <- ncol(grid); dr <- c(-1, 0, 1, 0); dc <- c(0, 1, 0, -1); opp <- c(3, 4, 1, 2)
min_s <- 0; max_s <- 3
dist <- array(Inf, dim = c(nr, nc, 4, max_s))
buckets <- vector("list", nr * nc * 9)
for (d in c(2, 3)) {
  rn <- 1 + dr[d]; cn <- 1 + dc[d]
  if (rn >= 1 && rn <= nr && cn >= 1 && cn <= nc) {
    v <- grid[rn, cn]; dist[rn, cn, d, 1] <- v
    buckets[[v]] <- c(buckets[[v]], rn + (cn - 1) * nr + (d - 1) * (nr * nc))
  }
}
f <- FALSE
for (cost in 1:length(buckets)) {
  if (f) break
  if (is.null(buckets[[cost]])) next
  for (idx in buckets[[cost]]) {
    t <- idx - 1; r <- t %% nr + 1; t <- t %/% nr; c <- t %% nc + 1; t <- t %/% nc
    d <- t %% 4 + 1; s <- t %/% 4 + 1
    if (cost > dist[r, c, d, s] || f) next
    if (r == nr && c == nc) { cat(cost, "\n"); f <- TRUE; break }
    for (nd in 1:4) {
      if (nd == opp[d]) next
      if (nd == d) ns <- s + 1 else { if (s < min_s) next; ns <- 1 }
      if (ns > max_s) next
      rn <- r + dr[nd]; cn <- c + dc[nd]
      if (rn >= 1 && rn <= nr && cn >= 1 && cn <= nc) {
        ncost <- cost + grid[rn, cn]
        if (ncost < dist[rn, cn, nd, ns]) {
          dist[rn, cn, nd, ns] <- ncost
          buckets[[ncost]] <- c(buckets[[ncost]], rn + (cn - 1) * nr + (nd - 1) * (nr * nc) + (ns - 1) * (nr * nc * 4))
        }
      }
    }
  }
}
