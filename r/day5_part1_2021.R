
input <- readLines("input.txt")
coords <- strsplit(input, " -> ")
starts <- lapply(coords, function(x) as.integer(strsplit(x[1], ",")[[1]]))
ends <- lapply(coords, function(x) as.integer(strsplit(x[2], ",")[[1]]))

grid <- matrix(0L, nrow = 1000, ncol = 1000)

for (i in seq_along(starts)) {
  x1 <- starts[[i]][1]
  y1 <- starts[[i]][2]
  x2 <- ends[[i]][1]
  y2 <- ends[[i]][2]
  
  if (x1 == x2) {
    if (y1 > y2) {
      temp <- y1
      y1 <- y2
      y2 <- temp
    }
    grid[x1, (y1:y2)] <- grid[x1, (y1:y2)] + 1L
  } else if (y1 == y2) {
    if (x1 > x2) {
      temp <- x1
      x1 <- x2
      x2 <- temp
    }
    grid[(x1:x2), y1] <- grid[(x1:x2), y1] + 1L
  }
}

overlapCount <- sum(grid > 1L)
cat(overlapCount, "\n")
