
buildGrid <- function(input, empty) {
  grid <- list(
    Width = nchar(input[1]),
    Height = length(input),
    Data = list()
  )

  for (y in 1:length(input)) {
    for (x in 1:nchar(input[y])) {
      char <- substr(input[y], x, x)
      if (char != empty) {
        grid$Data[[paste0(x-1, ",", y-1)]] <- char
      }
    }
  }

  return(grid)
}

toString <- function(grid, empty) {
  result <- ""

  for (y in 0:(grid$Height - 1)) {
    for (x in 0:(grid$Width - 1)) {
      coord <- paste0(x, ",", y)
      if (coord %in% names(grid$Data)) {
        result <- paste0(result, grid$Data[[coord]])
      } else {
        result <- paste0(result, empty)
      }
    }
    result <- paste0(result, "\n")
  }

  return(result)
}

getEmptyRows <- function(grid) {
  emptyRows <- c()
  for (y in 0:(grid$Height - 1)) {
    isEmpty <- TRUE

    for (x in 0:(grid$Width - 1)) {
      if (paste0(x, ",", y) %in% names(grid$Data)) {
        isEmpty <- FALSE
        break
      }
    }

    if (isEmpty) {
      emptyRows <- c(emptyRows, y)
    }
  }
  return(emptyRows)
}

getEmptyCols <- function(grid) {
  emptyCols <- c()
  for (x in 0:(grid$Width - 1)) {
    isEmpty <- TRUE

    for (y in 0:(grid$Height - 1)) {
      if (paste0(x, ",", y) %in% names(grid$Data)) {
        isEmpty <- FALSE
        break
      }
    }

    if (isEmpty) {
      emptyCols <- c(emptyCols, x)
    }
  }
  return(emptyCols)
}

calculateOffsets <- function(emptyIndexes, bound) {
  offsets <- rep(0, bound)
  for (idx in emptyIndexes) {
    for (i in (idx + 2):bound) {
      offsets[i] <- offsets[i] + 1
    }
  }
  return(offsets)
}

expandGrid <- function(grid, expansionFactor) {
  emptyCols <- getEmptyCols(grid)
  emptyRows <- getEmptyRows(grid)
  numLinesToAdd <- expansionFactor - 1

  newGrid <- list(
    Width = grid$Width + length(emptyCols)*numLinesToAdd,
    Height = grid$Height + length(emptyRows)*numLinesToAdd,
    Data = list()
  )

  dXs <- calculateOffsets(emptyCols, grid$Width)
  dYs <- calculateOffsets(emptyRows, grid$Height)

  for (y in 0:(grid$Height - 1)) {
    for (x in 0:(grid$Width - 1)) {
      coord <- paste0(x, ",", y)
      if (coord %in% names(grid$Data)) {
        newCoord <- paste0(x + dXs[x + 1]*numLinesToAdd, ",", y + dYs[y + 1]*numLinesToAdd)
        newGrid$Data[[newCoord]] <- grid$Data[[coord]]
      }
    }
  }

  return(newGrid)
}

abs <- function(x) {
  if (x < 0) {
    return(-x)
  } else {
    return(x)
  }
}

calculateLength <- function(grid, c1, c2) {
  dX <- abs(c2$X - c1$X)
  dY <- abs(c2$Y - c1$Y)
  return(dX + dY)
}

solve <- function(input, expansionFactor) {
  grid <- buildGrid(input, ".")

  expandedGrid <- expandGrid(grid, expansionFactor)

  res <- 0
  alreadySeen <- list()
  for (coord1 in names(expandedGrid$Data)) {
    for (coord2 in names(alreadySeen)) {
      c1 <- strsplit(coord1, ",")[[1]]
      c2 <- strsplit(coord2, ",")[[1]]
      length <- calculateLength(expandedGrid, list(X=as.integer(c1[1]), Y=as.integer(c1[2])), list(X=as.integer(c2[1]), Y=as.integer(c2[2])))
      res <- res + length
    }
    alreadySeen[[coord1]] <- TRUE
  }

  return(res)
}

input <- readLines("input.txt")
cat(solve(input, 1000000))
