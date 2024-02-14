
input <- readLines("input.txt")

parse_input <- function(input) {
  matrix(unlist(strsplit(input, "")), nrow = 100, byrow = TRUE)
}

count_neighbors <- function(grid, x, y) {
  sum(grid[max(1, x - 1):min(100, x + 1), max(1, y - 1):min(100, y + 1)] == "#") - (grid[x, y] == "#")
}

next_state <- function(grid) {
  new_grid <- matrix(0, nrow = 100, ncol = 100)
  for (i in 1:100) {
    for (j in 1:100) {
      neighbors_on <- count_neighbors(grid, i, j)
      if (grid[i, j] == "#" && neighbors_on != 2 && neighbors_on != 3) {
        new_grid[i, j] <- "."
      } else if (grid[i, j] == "." && neighbors_on == 3) {
        new_grid[i, j] <- "#"
      } else {
        new_grid[i, j] <- grid[i, j]
      }
    }
  }
  return(new_grid)
}

grid <- parse_input(input)

for (i in 1:100) {
  grid <- next_state(grid)
}

sum(grid == "#")
