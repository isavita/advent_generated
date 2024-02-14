
Side <- 5
Square <- Side * Side

parse <- function() {
  res <- rep(FALSE, Square)
  
  lines <- readLines("input.txt")
  for (row in 1:Side) {
    line <- lines[row]
    for (col in 1:Side) {
      if (substr(line, col, col) == "#") {
        res[(row-1)*Side + col] <- TRUE
      } else {
        res[(row-1)*Side + col] <- FALSE
      }
    }
  }
  return(res)
}

next1 <- function(grid) {
  newGrid <- rep(FALSE, Square)
  
  for (i in 1:Square) {
    row <- floor((i-1)/Side) + 1
    col <- (i-1) %% Side + 1
    neighbours <- 0
    
    if (row > 1 && grid[i-Side]) {
      neighbours <- neighbours + 1
    }
    if (row < Side && grid[i+Side]) {
      neighbours <- neighbours + 1
    }
    if (col > 1 && grid[i-1]) {
      neighbours <- neighbours + 1
    }
    if (col < Side && grid[i+1]) {
      neighbours <- neighbours + 1
    }
    
    if (grid[i] && neighbours != 1) {
      newGrid[i] <- FALSE
      next
    }
    
    if (!grid[i] && (neighbours == 1 || neighbours == 2)) {
      newGrid[i] <- TRUE
      next
    }
    
    newGrid[i] <- grid[i]
  }
  
  return(newGrid)
}

biodiversity <- function(grid) {
  bio <- 0
  for (i in 1:Square) {
    if (grid[i]) {
      bio <- bio + 2^(i-1)
    }
  }
  return(bio)
}

appeared <- list()

grid <- parse()
appeared[[paste(grid, collapse = "")]] <- TRUE
while (TRUE) {
  grid <- next1(grid)
  if (paste(grid, collapse = "") %in% names(appeared)) {
    print(biodiversity(grid))
    break
  }
  appeared[[paste(grid, collapse = "")]] <- TRUE
}

