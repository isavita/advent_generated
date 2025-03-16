
solve <- function() {
  grid <- readLines("input.txt")
  rows <- length(grid)
  cols <- nchar(grid[1])
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  total_price <- 0

  for (r in 1:rows) {
    for (c in 1:cols) {
      if (!visited[r, c]) {
        region <- calculate_region(grid, r, c, visited)
        visited <- region$visited
        total_price <- total_price + region$area * region$perimeter
      }
    }
  }
  return(total_price)
}

calculate_region <- function(grid, row, col, visited) {
    rows <- length(grid)
    cols <- nchar(grid[1])
    char <- substr(grid[row], col, col)
    area <- 0
    perimeter <- 0
    queue <- list(c(row, col))
    visited[row, col] <- TRUE
  
    while (length(queue) > 0) {
        rc <- queue[[1]]
        queue <- queue[-1]
        r <- rc[1]
        c <- rc[2]
    
        area <- area + 1

        is_border <- r == 1 || r == rows || c == 1 || c == cols
        
        for (dir in list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0))) {
          dr <- dir[1]
          dc <- dir[2]
          nr <- r + dr
          nc <- c + dc
          
          if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
            if (substr(grid[nr], nc, nc) != char) {
              perimeter <- perimeter + 1
            } else if (!visited[nr, nc]) {
                visited[nr, nc] <- TRUE
                queue[[length(queue) + 1]] <- c(nr, nc)
            }
          } else if (is_border){
            perimeter <- perimeter + 1
          }
        }
    }
    return(list(area = area, perimeter = perimeter, visited = visited))
}

main <- function() {
    cat(solve(), "\n")
}

main()
