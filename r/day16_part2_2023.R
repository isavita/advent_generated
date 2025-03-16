
# Helper function to simulate the beam's path and count energized tiles
simulate_beam <- function(grid, start_row, start_col, start_dr, start_dc) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  energized <- matrix(FALSE, rows, cols)
  visited <- array(FALSE, dim = c(rows, cols, 4)) # Track visited directions (up, right, down, left) = (1,2,3,4)
  
  # Use a stack (list) for DFS
  stack <- list(list(r = start_row, c = start_col, dr = start_dr, dc = start_dc))
  
  while (length(stack) > 0) {
    current <- stack[[1]]
    stack <- stack[-1]
    
    r <- current$r
    c <- current$c
    dr <- current$dr
    dc <- current$dc
    
    # Check bounds
    if (r < 1 || r > rows || c < 1 || c > cols) {
      next
    }
    
    # Determine direction index for visited tracking
    dir_index <- (if (dr == -1) 1 else if (dc == 1) 2 else if (dr == 1) 3 else 4)

    # Check if visited in this direction
    if (visited[r, c, dir_index]) {
      next
    }
    visited[r, c, dir_index] <- TRUE
    energized[r, c] <- TRUE

    
    char <- grid[r, c]
    
    if (char == '.') {
      stack <- c(stack, list(list(r = r + dr, c = c + dc, dr = dr, dc = dc)))
    } else if (char == '/') {
      new_dr <- -dc
      new_dc <- -dr
      stack <- c(stack, list(list(r = r + new_dr, c = c + new_dc, dr = new_dr, dc = new_dc)))
    } else if (char == '\\') {
      new_dr <- dc
      new_dc <- dr
      stack <- c(stack, list(list(r = r + new_dr, c = c + new_dc, dr = new_dr, dc = new_dc)))
    } else if (char == '|') {
      if (dc != 0) { # Split vertically
        stack <- c(stack, list(list(r = r - 1, c = c, dr = -1, dc = 0)))
        stack <- c(stack, list(list(r = r + 1, c = c, dr = 1, dc = 0)))
      } else { # Pass through
        stack <- c(stack, list(list(r = r + dr, c = c + dc, dr = dr, dc = dc)))
      }
    } else if (char == '-') {
      if (dr != 0) { # Split horizontally
        stack <- c(stack, list(list(r = r, c = c - 1, dr = 0, dc = -1)))
        stack <- c(stack, list(list(r = r, c = c + 1, dr = 0, dc = 1)))
      } else { # Pass through
        stack <- c(stack, list(list(r = r + dr, c = c + dc, dr = dr, dc = dc)))
      }
    }
  }
  
  return(sum(energized))
}


main <- function() {
  # Read the input grid from file
  input_lines <- readLines("input.txt")
  grid <- do.call(rbind, lapply(strsplit(input_lines, ""), function(x) x))
  
  # Part 1
  energized_count <- simulate_beam(grid, 1, 1, 0, 1)
  cat("Part 1: Number of energized tiles:", energized_count, "\n")
  
  # Part 2
  rows <- nrow(grid)
  cols <- ncol(grid)
  max_energized <- 0
  
  # Top and bottom edges
  for (c in 1:cols) {
    max_energized <- max(max_energized, simulate_beam(grid, 1, c, 1, 0))  # From top
    max_energized <- max(max_energized, simulate_beam(grid, rows, c, -1, 0)) # From bottom
  }
  
  # Left and right edges
  for (r in 1:rows) {
    max_energized <- max(max_energized, simulate_beam(grid, r, 1, 0, 1))  # From left
    max_energized <- max(max_energized, simulate_beam(grid, r, cols, 0, -1)) # From right
  }
  
  cat("Part 2: Maximum number of energized tiles:", max_energized, "\n")
}

main()
