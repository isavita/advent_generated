input <- readLines("input.txt")
stars <- regmatches(input, regexec("position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>", input))
stars <- do.call(rbind, lapply(stars, function(x) as.integer(x[2:5])))
stars <- as.data.frame(stars)

smallestT <- 0
smallestArea <- Inf

for (t in 1:100000) {
  coords <- stars
  coords[, 1] <- coords[, 1] + coords[, 3] * t
  coords[, 2] <- coords[, 2] + coords[, 4] * t
  
  maxX <- max(coords[, 1])
  minX <- min(coords[, 1])
  maxY <- max(coords[, 2])
  minY <- min(coords[, 2])
  
  area <- (maxX - minX + 1) * (maxY - minY + 1)
  
  if (area < smallestArea) {
    smallestArea <- area
    smallestT <- t
  }
}

coords <- stars
coords[, 1] <- coords[, 1] + coords[, 3] * smallestT
coords[, 2] <- coords[, 2] + coords[, 4] * smallestT

maxX <- max(coords[, 1])
minX <- min(coords[, 1])
maxY <- max(coords[, 2])
minY <- min(coords[, 2])

mapper <- matrix(FALSE, nrow = maxY - minY + 1, ncol = maxX - minX + 1)

for (i in 1:nrow(coords)) {
  mapper[coords[i, 2] - minY + 1, coords[i, 1] - minX + 1] <- TRUE
}

print(smallestT)