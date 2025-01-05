
input <- readLines("input.txt")
lines <- strsplit(input, " -> ")
coords <- lapply(lines, function(x) {
  start <- as.integer(strsplit(x[1], ",")[[1]])
  end <- as.integer(strsplit(x[2], ",")[[1]])
  c(start, end)
})
coords <- do.call(rbind, coords)

overlaps <- matrix(0, max(coords[,c(1,3)]) + 1, max(coords[,c(2,4)]) + 1)

for (i in 1:nrow(coords)) {
  x1 <- coords[i, 1]
  y1 <- coords[i, 2]
  x2 <- coords[i, 3]
  y2 <- coords[i, 4]
  
  xStep <- sign(x2 - x1)
  yStep <- sign(y2 - y1)
  steps <- max(abs(x2 - x1), abs(y2 - y1)) + 1
  
  for (j in 0:(steps - 1)) {
    x <- x1 + j * xStep
    y <- y1 + j * yStep
    overlaps[x + 1, y + 1] <- overlaps[x + 1, y + 1] + 1
  }
}

print(sum(overlaps > 1))
