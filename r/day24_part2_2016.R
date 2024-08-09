cleaningRobot <- function(input) {
  grid <- strsplit(input, "\n")[[1]]
  grid <- lapply(grid, function(x) strsplit(x, "")[[1]])
  
  graph <- list()
  for (r in seq_along(grid)) {
    for (c in seq_along(grid[[r]])) {
      cell <- grid[[r]][[c]]
      if (grepl("[0-9]", cell)) {
        poi <- cell
        distancesFromPOI <- bfsGetEdgeWeights(grid, c(r, c))
        index <- as.integer(poi)
        graph[[index + 1]] <- distancesFromPOI
      }
    }
  }
  
  return(dfs(graph, 1, c(1), TRUE))
}

bfsGetEdgeWeights <- function(grid, start) {
  poiToDistance <- setNames(0, grid[[start[1]]][[start[2]]])
  queue <- list(list(row = start[1], col = start[2], distance = 0))
  visited <- matrix(FALSE, nrow = length(grid), ncol = length(grid[[1]]))
  
  while (length(queue) > 0) {
    front <- queue[[1]]
    queue <- queue[-1]
    
    if (visited[front$row, front$col]) next
    visited[front$row, front$col] <- TRUE
    
    if (grepl("[0-9]", grid[[front$row]][[front$col]])) {
      poiToDistance[grid[[front$row]][[front$col]]] <- front$distance
    }
    
    for (d in list(c(0, -1), c(0, 1), c(1, 0), c(-1, 0))) {
      nextRow <- front$row + d[1]
      nextCol <- front$col + d[2]
      if (nextRow > 0 && nextRow <= length(grid) && nextCol > 0 && nextCol <= length(grid[[1]]) && grid[[nextRow]][[nextCol]] != "#") {
        queue <- append(queue, list(list(row = nextRow, col = nextCol, distance = front$distance + 1)))
      }
    }
  }
  
  distances <- integer(length(poiToDistance))
  for (numStr in names(poiToDistance)) {
    n <- as.integer(numStr)
    distances[n + 1] <- poiToDistance[numStr]
  }
  return(distances)
}

dfs <- function(graph, entryIndex, visited, returnToZero) {
  if (length(graph) == length(visited)) {
    return(if (returnToZero) graph[[entryIndex]][[1]] else 0)
  }
  
  minDistance <- Inf
  for (i in seq_along(graph[[entryIndex]])) {
    if (!(i %in% visited)) {
      visited <- c(visited, i)
      dist <- graph[[entryIndex]][[i]] + dfs(graph, i, visited, returnToZero)
      minDistance <- min(minDistance, dist)
      visited <- visited[-length(visited)]
    }
  }
  
  return(minDistance)
}

readFile <- function(pathFromCaller) {
  con <- file(pathFromCaller, "r")
  content <- readLines(con)
  close(con)
  return(paste(content, collapse = "\n"))
}

input <- readFile("input.txt")
result <- cleaningRobot(input)
print(result)