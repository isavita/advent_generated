
input <- readLines("input.txt")

grid <- matrix(unlist(strsplit(input, "")), nrow = 100, byrow = TRUE)

corners_on <- function(grid) {
  grid[1, 1] <- "#"
  grid[1, 100] <- "#"
  grid[100, 1] <- "#"
  grid[100, 100] <- "#"
  
  return(grid)
}

count_neighbors <- function(grid, i, j) {
  count <- 0
  
  for (x in -1:1) {
    for (y in -1:1) {
      if (x == 0 & y == 0) next
      
      if (i + x < 1 | i + x > 100 | j + y < 1 | j + y > 100) next
      
      if (grid[i + x, j + y] == "#") count <- count + 1
    }
  }
  
  return(count)
}

update_grid <- function(grid) {
  new_grid <- matrix("", nrow = 100, ncol = 100)
  
  for (i in 1:100) {
    for (j in 1:100) {
      neighbors_on <- count_neighbors(grid, i, j)
      
      if (grid[i, j] == "#" & (neighbors_on == 2 | neighbors_on == 3)) {
        new_grid[i, j] <- "#"
      } else if (grid[i, j] == "." & neighbors_on == 3) {
        new_grid[i, j] <- "#"
      } else {
        new_grid[i, j] <- "."
      }
    }
  }
  
  return(new_grid)
}

grid <- corners_on(grid)

for (step in 1:100) {
  grid <- update_grid(grid)
  grid <- corners_on(grid)
}

answer <- sum(grid == "#")
print(answer)
