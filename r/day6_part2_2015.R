gridSize <- 1000
grid <- matrix(0, nrow = gridSize, ncol = gridSize)

instructions <- readLines("input.txt")
for (instruction in instructions) {
  parts <- strsplit(instruction, "\\s+")[[1]]
  if (grepl("^turn on", instruction)) {
    action <- 1
  } else if (grepl("^turn off", instruction)) {
    action <- -1
  } else if (grepl("^toggle", instruction)) {
    action <- 2
  }
  
  coords <- strsplit(parts[length(parts)-2], ",")[[1]]
  startX <- as.integer(coords[1])
  startY <- as.integer(coords[2])
  
  coords <- strsplit(parts[length(parts)], ",")[[1]]
  endX <- as.integer(coords[1])
  endY <- as.integer(coords[2])
  
  for (x in startX:endX) {
    for (y in startY:endY) {
      grid[x, y] <- grid[x, y] + action
      if (grid[x, y] < 0) {
        grid[x, y] <- 0
      }
    }
  }
}

brightness <- sum(grid)
print(brightness)