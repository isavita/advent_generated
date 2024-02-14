
data <- readLines("input.txt")
grid <- lapply(data, function(x) strsplit(x, "")[[1]])

transform <- function(grid) {
  newGrid <- lapply(grid, function(x) x)
  for (i in seq_along(grid)) {
    newGrid[[i]] <- sapply(seq_along(grid[[i]]), function(j) nextAcreState(grid, i, j))
  }
  newGrid
}

nextAcreState <- function(grid, i, j) {
  acreType <- grid[[i]][[j]]
  if (acreType == '.') {
    if (countAdjacent(grid, i, j, '|') >= 3) {
      return('|')
    }
  } else if (acreType == '|') {
    if (countAdjacent(grid, i, j, '#') >= 3) {
      return('#')
    }
  } else if (acreType == '#') {
    if (countAdjacent(grid, i, j, '#') >= 1 && countAdjacent(grid, i, j, '|') >= 1) {
      return('#')
    } else {
      return('.')
    }
  }
  acreType
}

countAdjacent <- function(grid, i, j, acreType) {
  count <- 0
  for (x in -1:1) {
    for (y in -1:1) {
      if (x == 0 && y == 0) {
        next
      }
      if (i + x >= 1 && i + x <= length(grid) && j + y >= 1 && j + y <= length(grid[[i]])) {
        if (grid[[i + x]][[j + y]] == acreType) {
          count <- count + 1
        }
      }
    }
  }
  count
}

countResources <- function(grid) {
  wooded <- 0
  lumberyards <- 0
  for (i in seq_along(grid)) {
    for (j in seq_along(grid[[i]])) {
      acreType <- grid[[i]][[j]]
      if (acreType == '|') {
        wooded <- wooded + 1
      } else if (acreType == '#') {
        lumberyards <- lumberyards + 1
      }
    }
  }
  c(wooded, lumberyards)
}

for (minute in 1:10) {
  grid <- transform(grid)
}

resources <- countResources(grid)
cat(resources[1] * resources[2])
