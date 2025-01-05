
input <- readLines("input.txt")
n <- length(input)
m <- nchar(input[1])
grid <- matrix(unlist(strsplit(input, "")), nrow = n, byrow = TRUE)
start <- which(grid == "S", arr.ind = TRUE)
end <- which(grid == "E", arr.ind = TRUE)
sx <- start[1]
sy <- start[2]
ex <- end[1]
ey <- end[2]
dx <- c(-1, 0, 1, 0)
dy <- c(0, 1, 0, -1)
dist <- array(Inf, dim = c(n, m, 4))
dist[sx, sy, 2] <- 0
pq <- data.frame(x = sx, y = sy, d = 2, cost = 0)
while (nrow(pq) > 0) {
  u <- pq[1,]
  pq <- pq[-1,]
  if (dist[u$x, u$y, u$d] < u$cost) {
    next
  }
  if (u$x == ex && u$y == ey) {
    cat(u$cost, "\n")
    quit()
  }
  ndirs <- c((u$d + 1) %% 4, (u$d + 3) %% 4)
  ndirs[ndirs == 0] <- 4
  for (ndir in ndirs) {
    nc <- u$cost + 1000
    if (nc < dist[u$x, u$y, ndir]) {
      dist[u$x, u$y, ndir] <- nc
      pq <- rbind(pq, data.frame(x = u$x, y = u$y, d = ndir, cost = nc))
    }
  }
  nx <- u$x + dx[u$d]
  ny <- u$y + dy[u$d]
  if (nx >= 1 && nx <= n && ny >= 1 && ny <= m && grid[nx, ny] != "#") {
    nc <- u$cost + 1
    if (nc < dist[nx, ny, u$d]) {
      dist[nx, ny, u$d] <- nc
      pq <- rbind(pq, data.frame(x = nx, y = ny, d = u$d, cost = nc))
    }
  }
  pq <- pq[order(pq$cost),]
}
