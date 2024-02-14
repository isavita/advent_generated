
data <- readLines("input.txt")

points <- list()
folds <- list()

readingPoints <- TRUE

for (line in data) {
  if (line == "") {
    readingPoints <- FALSE
    next
  }
  
  if (readingPoints) {
    parts <- unlist(strsplit(line, ","))
    x <- as.integer(parts[1])
    y <- as.integer(parts[2])
    points[[length(points) + 1]] <- c(x, y)
  } else {
    parts <- unlist(strsplit(line, "="))
    val <- as.integer(parts[2])
    if (grepl("x", parts[1])) {
      folds[[length(folds) + 1]] <- c(val, 0)
    } else {
      folds[[length(folds) + 1]] <- c(0, val)
    }
  }
}

for (i in seq_along(folds)) {
  newPoints <- list()
  for (point in points) {
    newPoint <- point
    if (folds[[i]][1] != 0 && point[1] > folds[[i]][1]) {
      newPoint[1] <- folds[[i]][1] - (point[1] - folds[[i]][1])
    } else if (folds[[i]][2] != 0 && point[2] > folds[[i]][2]) {
      newPoint[2] <- folds[[i]][2] - (point[2] - folds[[i]][2])
    }
    newPoints[[length(newPoints) + 1]] <- newPoint
  }
  points <- newPoints
  if (i == 1) {
    cat("Number of dots visible after first fold:", length(points), "\n")
  }
}

maxX <- 0
maxY <- 0
for (point in points) {
  if (point[1] > maxX) {
    maxX <- point[1]
  }
  if (point[2] > maxY) {
    maxY <- point[2]
  }
}

grid <- matrix(" ", nrow = maxY + 1, ncol = maxX + 1)

for (point in points) {
  grid[point[2] + 1, point[1] + 1] <- "#"
}

for (i in 1:nrow(grid)) {
  cat(paste(grid[i, ], collapse = ""), "\n")
}
