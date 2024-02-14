
content <- readLines("input.txt")
coordinates <- lapply(content, function(x) as.integer(strsplit(x, ", ")[[1]]))

minX <- min(sapply(coordinates, function(x) x[1]))
minY <- min(sapply(coordinates, function(x) x[2]))
maxX <- max(sapply(coordinates, function(x) x[1]))
maxY <- max(sapply(coordinates, function(x) x[2]))

regionSize <- 0

for (x in minX:maxX) {
  for (y in minY:maxY) {
    totalDistance <- 0
    
    for (c in coordinates) {
      totalDistance <- totalDistance + abs(x - c[1]) + abs(y - c[2])
    }
    
    if (totalDistance < 10000) {
      regionSize <- regionSize + 1
    }
  }
}

print(regionSize)
