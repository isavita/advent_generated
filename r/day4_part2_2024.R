
checkMAS <- function(grid, x, y, dx, dy) {
  n_rows <- nrow(grid)
  n_cols <- ncol(grid)
  word <- c("M", "A", "S")
  
  forward <- TRUE
  backward <- TRUE
  
  for (i in 1:length(word)) {
    new_x <- x + (dx * (i - 1))
    new_y <- y + (dy * (i - 1))
    if (new_x < 1 || new_y < 1 || new_x > n_rows || new_y > n_cols) {
      forward <- FALSE
      break
    }
    if (grid[new_x, new_y] != word[i]) {
      forward <- FALSE
    }
  }
  
  for (i in 1:length(word)) {
    new_x <- x + (dx * (i - 1))
    new_y <- y + (dy * (i - 1))
    if (new_x < 1 || new_y < 1 || new_x > n_rows || new_y > n_cols) {
      backward <- FALSE
      break
    }
    if (grid[new_x, new_y] != word[length(word) - i + 1]) {
      backward <- FALSE
    }
  }
  
  return(forward || backward)
}

checkXMAS <- function(grid, x, y) {
  if (checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1)) {
    return(TRUE)
  }
  if (checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1)) {
    return(TRUE)
  }
  return(FALSE)
}

countXMASPatterns <- function(grid) {
  n_rows <- nrow(grid)
  n_cols <- ncol(grid)
  count <- 0
  
  if (n_rows < 3 || n_cols < 3) {
    return(0)
  }
  
  for (i in 2:(n_rows - 1)) {
    for (j in 2:(n_cols - 1)) {
      if (grid[i, j] == "A" && checkXMAS(grid, i, j)) {
        count <- count + 1
      }
    }
  }
  return(count)
}

input <- readLines("input.txt")
input <- input[input != ""]
grid <- do.call(rbind, strsplit(input, ""))
count <- countXMASPatterns(grid)
cat("X-MAS patterns appear", count, "times in the word search\n")
