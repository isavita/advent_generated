
# Function to calculate the expanded grid
expand_grid <- function(grid) {
  n_row <- nrow(grid)
  n_col <- ncol(grid)
  expanded_grid <- matrix(0, nrow = n_row * 5, ncol = n_col * 5)
  
  for (i in 0:4) {
    for (j in 0:4) {
      temp_grid <- grid + i + j
      temp_grid[temp_grid > 9] <- temp_grid[temp_grid > 9] - 9
      expanded_grid[(i * n_row + 1):((i + 1) * n_row), (j * n_col + 1):((j + 1) * n_col)] <- temp_grid
    }
  }
  return(expanded_grid)
}

# Dijkstra's algorithm to find the shortest path
dijkstra <- function(grid) {
    rows <- nrow(grid)
    cols <- ncol(grid)
    
    # Initialize distance matrix with Inf, except starting point
    dist_matrix <- matrix(Inf, rows, cols)
    dist_matrix[1, 1] <- 0
    
    # Initialize visited matrix
    visited <- matrix(FALSE, rows, cols)
    
    # Priority queue (using a list for simplicity and efficiency)
    queue <- list(c(1, 1, 0)) # row, col, distance
    
    # Possible moves (up, down, left, right)
    moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
    
    while (length(queue) > 0) {
        # Find the node with the smallest distance
        min_dist <- Inf
        min_index <- -1
        
        for (i in 1:length(queue)) {
            if (queue[[i]][3] < min_dist) {
                min_dist <- queue[[i]][3]
                min_index <- i
            }
        }
        
        current <- queue[[min_index]]
        queue[[min_index]] <- NULL  # Remove the element
        
        
        row <- current[1]
        col <- current[2]
        
        if (visited[row, col]) {
            next
        }
        visited[row, col] <- TRUE
        
        # Check neighbors
        for (move in moves) {
            new_row <- row + move[1]
            new_col <- col + move[2]
            
            # Check boundaries
            if (new_row >= 1 && new_row <= rows && new_col >= 1 && new_col <= cols) {
                new_dist <- dist_matrix[row, col] + grid[new_row, new_col]
                
                if (new_dist < dist_matrix[new_row, new_col]) {
                    dist_matrix[new_row, new_col] <- new_dist
                    queue[[length(queue) + 1]] <- c(new_row, new_col, new_dist) # Add to queue
                }
            }
        }
    }
    
    return(dist_matrix[rows, cols])
}



main <- function() {
  # Read the input grid from file
  input_data <- readLines("input.txt")
  grid <- do.call(rbind, lapply(strsplit(input_data, ""), as.integer))
  
  # Part 1: Find lowest risk on original grid
  lowest_risk_part1 <- dijkstra(grid)
  cat("Lowest total risk (Part 1):", lowest_risk_part1, "\n")
  
  # Part 2: Expand the grid and find lowest risk
  expanded_grid <- expand_grid(grid)
  lowest_risk_part2 <- dijkstra(expanded_grid)
  cat("Lowest total risk (Part 2):", lowest_risk_part2, "\n")
}

# Set main as the entry point
if (sys.nframe() == 0) {
  main()
}
