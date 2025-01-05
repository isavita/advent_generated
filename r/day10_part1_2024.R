
input <- readLines("input.txt")
grid <- do.call(rbind, lapply(strsplit(input, ""), as.integer))
nr <- nrow(grid)
nc <- ncol(grid)
dirs <- list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))
trailheads <- which(grid == 0, arr.ind = TRUE)
sumScores <- 0

for (i in 1:nrow(trailheads)) {
  th <- trailheads[i, ]
  reached <- matrix(FALSE, nrow = nr, ncol = nc)
  front <- list(list(p = th, h = 0))
  visited <- list()
  while (length(front) > 0) {
    cur <- front[[length(front)]]
    front <- front[-length(front)]
    if (cur$h == 9) {
      if (!reached[cur$p[1], cur$p[2]]) {
        reached[cur$p[1], cur$p[2]] <- TRUE
      }
      next
    }
    for (d in dirs) {
      nr2 <- cur$p[1] + d[1]
      nc2 <- cur$p[2] + d[2]
      if (nr2 < 1 || nr2 > nr || nc2 < 1 || nc2 > nc) next
      if (grid[nr2, nc2] == cur$h + 1) {
        key <- paste(nr2, nc2, cur$h + 1, sep = ",")
        if (!key %in% visited) {
          visited <- c(visited, key)
          front <- c(front, list(list(p = c(nr2, nc2), h = cur$h + 1)))
        }
      }
    }
  }
  sumScores <- sumScores + sum(reached)
}
cat(sumScores, "\n")
