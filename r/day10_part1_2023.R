
# Function to find the starting position 'S'
find_start <- function(grid) {
    for (i in 1:nrow(grid)) {
        for (j in 1:ncol(grid)) {
            if (grid[i, j] == 'S') {
                return(c(i, j))
            }
        }
    }
}

# Function to determine valid neighbors based on pipe type
get_neighbors <- function(grid, pos) {
  i <- pos[1]
  j <- pos[2]
  pipe <- grid[i, j]
  neighbors <- list()
  
  # Define possible moves based on pipe type
  if (pipe == '|' || pipe == 'S') {  # Vertical pipe
      neighbors <- c(neighbors, list(c(i - 1, j)), list(c(i + 1, j)))
  }
  if (pipe == '-' || pipe == 'S') {  # Horizontal pipe
      neighbors <- c(neighbors, list(c(i, j - 1)), list(c(i, j + 1)))
  }
  if (pipe == 'L' || pipe == 'S') {  # North and East
      neighbors <- c(neighbors, list(c(i - 1, j)), list(c(i, j + 1)))
  }
  if (pipe == 'J' || pipe == 'S') {  # North and West
      neighbors <- c(neighbors, list(c(i - 1, j)), list(c(i, j - 1)))
  }
  if (pipe == '7' || pipe == 'S') {  # South and West
      neighbors <- c(neighbors, list(c(i + 1, j)), list(c(i, j - 1)))
  }
  if (pipe == 'F' || pipe == 'S') {  # South and East
      neighbors <- c(neighbors, list(c(i + 1, j)), list(c(i, j + 1)))
  }
    
    # Filter out neighbors that are out of bounds or invalid next steps
    valid_neighbors <- list()
    for (neighbor in neighbors) {
        ni <- neighbor[1]
        nj <- neighbor[2]
    
        if (ni >= 1 && ni <= nrow(grid) && nj >= 1 && nj <= ncol(grid)) {
            
            next_pipe <- grid[ni,nj]
            
            # Check if connecting pipe makes sense
            
            # Came from NORTH
            if(i - ni == 1){
                if(next_pipe %in% c("|", "7", "F"))
                    valid_neighbors <- c(valid_neighbors, list(neighbor))
            }
            # Came from SOUTH
            if(i - ni == -1){
                if(next_pipe %in% c("|", "L", "J"))
                    valid_neighbors <- c(valid_neighbors, list(neighbor))
            }
            # Came from WEST
            if(j - nj == 1){
                if(next_pipe %in% c("-", "L", "F"))
                    valid_neighbors <- c(valid_neighbors, list(neighbor))
            }
            # Came from EAST
            if(j - nj == -1){
                if(next_pipe %in% c("-", "J", "7"))
                    valid_neighbors <- c(valid_neighbors, list(neighbor))
            }
        }
    }

  return(valid_neighbors)
}


# Main function to calculate the farthest distance
solve_day10 <- function(file_path) {
    # Read the grid from the file
    lines <- readLines(file_path)
    grid <- do.call(rbind, strsplit(lines, ""))

    # Find the starting position
    start_pos <- find_start(grid)

    # Initialize distances and queue for BFS
    distances <- matrix(-1, nrow = nrow(grid), ncol = ncol(grid))
    distances[start_pos[1], start_pos[2]] <- 0
    queue <- list(start_pos)

    # Perform BFS
    while (length(queue) > 0) {
        current_pos <- queue[[1]]
        queue <- queue[-1]

        neighbors <- get_neighbors(grid, current_pos)

        for (neighbor in neighbors) {
            ni <- neighbor[1]
            nj <- neighbor[2]

            if (distances[ni, nj] == -1) {
                distances[ni, nj] <- distances[current_pos[1], current_pos[2]] + 1
                queue <- c(queue, list(neighbor))
            }
        }
    }

    # Find the maximum distance
    max_distance <- max(distances)
    return(max_distance)
}

# Main entry point
main <- function() {
    result <- solve_day10("input.txt")
    cat(result, "\n")
}

# Execute the main function
main()
