
# Function to simulate the falling bytes and find the shortest path
solve <- function(input_file) {
  # Read the byte positions from the input file
  byte_positions <- read.table(input_file, sep = ",", col.names = c("X", "Y"))

  # Set the dimensions of the memory space
  grid_size <- 71  # 0 to 70, inclusive

  # Initialize the grid with all safe spaces (0)
  grid <- matrix(0, nrow = grid_size, ncol = grid_size)

  # Function to calculate the shortest path using BFS
  find_shortest_path <- function(grid) {
    
    if (grid[1,1] == 1 || grid[grid_size, grid_size] == 1){
        return(Inf)
    }

    queue <- list(c(1, 1, 0))  # (row, col, steps)
    visited <- matrix(FALSE, nrow = grid_size, ncol = grid_size)
    visited[1, 1] <- TRUE

    while (length(queue) > 0) {
      current <- queue[[1]]
      queue <- queue[-1]

      row <- current[1]
      col <- current[2]
      steps <- current[3]

      if (row == grid_size && col == grid_size) {
        return(steps)
      }

      # Possible moves (up, down, left, right)
      moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))

      for (move in moves) {
        new_row <- row + move[1]
        new_col <- col + move[2]

        # Check boundaries and if the cell is not corrupted and not visited
        if (new_row >= 1 && new_row <= grid_size &&
            new_col >= 1 && new_col <= grid_size &&
            grid[new_row, new_col] == 0 &&
            !visited[new_row, new_col]) {

          queue <- c(queue, list(c(new_row, new_col, steps + 1)))
          visited[new_row, new_col] <- TRUE
        }
      }
    }
    return(Inf) # No path found
  }

  # Simulate the first 1024 bytes falling
    min_steps <- Inf
    temp_grid <- grid
    
    for (i in 1:min(1024,nrow(byte_positions))) {
        
        temp_grid[byte_positions$Y[i] + 1, byte_positions$X[i] + 1] <- 1  # Mark as corrupted
        
        current_steps <- find_shortest_path(temp_grid)
        
        if (!is.infinite(current_steps))
        {
            min_steps <- min(min_steps, current_steps)
        }
    }

  # Find the shortest path after simulating
  final_steps <- find_shortest_path(temp_grid)
    if (is.infinite(final_steps))
    {
        print("No path found")
    }else{
        print(final_steps)
    }
}

# Main function
main <- function() {
  input_file <- "input.txt"
  solve(input_file)
}

# Run the main function
main()
