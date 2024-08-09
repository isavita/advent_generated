points <- read.table("input.txt", sep = ",", col.names = c("X", "Y"))
maxX <- max(points$X)
maxY <- max(points$Y)
grid <- matrix(0, nrow = maxX + 2, ncol = maxY + 2)
areas <- integer(nrow(points))
infinite <- logical(nrow(points))

for (i in 1:(maxX + 2)) {
  for (j in 1:(maxY + 2)) {
    minDist <- maxX + maxY
    closest <- -1
    for (k in 1:nrow(points)) {
      dist <- abs(points$X[k] - (i - 1)) + abs(points$Y[k] - (j - 1))
      if (dist < minDist) {
        minDist <- dist
        closest <- k
      } else if (dist == minDist) {
        closest <- -1
      }
    }
    if (closest != -1) {
      if (i == 1 || j == 1 || i == maxX + 2 || j == maxY + 2) {
        infinite[closest] <- TRUE
      }
      areas[closest] <- areas[closest] + 1
    }
  }
}

maxArea <- max(areas[!infinite])
print(maxArea)