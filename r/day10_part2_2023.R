
# Function to find the starting position 'S'
find_start <- function(maze) {
    for (i in 1:nrow(maze)) {
        for (j in 1:ncol(maze)) {
            if (maze[i, j] == 'S') {
                return(c(i, j))
            }
        }
    }
}

# Function to determine valid neighbors based on pipe type
get_neighbors <- function(maze, pos) {
    row <- pos[1]
    col <- pos[2]
    neighbors <- list()
    pipe <- maze[row, col]
    
    # Check North
    if (row > 1 && (pipe %in% c('S', '|', 'L', 'J')) && (maze[row - 1, col] %in% c('|', '7', 'F'))) {
        neighbors <- c(neighbors, list(c(row - 1, col)))
    }
    # Check South
    if (row < nrow(maze) && (pipe %in% c('S', '|', '7', 'F')) && (maze[row + 1, col] %in% c('|', 'L', 'J'))) {
        neighbors <- c(neighbors, list(c(row + 1, col)))
    }
    # Check West
    if (col > 1 && (pipe %in% c('S', '-', 'J', '7')) && (maze[row, col - 1] %in% c('-', 'L', 'F'))) {
        neighbors <- c(neighbors, list(c(row, col - 1)))
    }
    # Check East
    if (col < ncol(maze) && (pipe %in% c('S', '-', 'L', 'F')) && (maze[row, col + 1] %in% c('-', 'J', '7'))) {
        neighbors <- c(neighbors, list(c(row, col + 1)))
    }
    
    return(neighbors)
}


# Function to calculate farthest distance in the loop (Part 1)
calculate_farthest_distance <- function(maze) {
    start_pos <- find_start(maze)
    queue <- list(list(pos = start_pos, dist = 0))
    visited <- matrix(FALSE, nrow = nrow(maze), ncol = ncol(maze))
    visited[start_pos[1], start_pos[2]] <- TRUE
    max_dist <- 0
    
    while (length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]
        max_dist <- max(max_dist, current$dist)
        
        neighbors <- get_neighbors(maze, current$pos)
        for (neighbor in neighbors) {
            if (!visited[neighbor[1], neighbor[2]]) {
                visited[neighbor[1], neighbor[2]] <- TRUE
                queue <- c(queue, list(list(pos = neighbor, dist = current$dist + 1)))
            }
        }
    }
    
    return(list(max_dist = max_dist, visited = visited))
}

# Function to calculate enclosed tiles (Part 2)
calculate_enclosed_tiles <- function(maze, loop_mask) {
  
    expanded_maze <- matrix('.', nrow = 2 * nrow(maze) , ncol = 2 * ncol(maze))
    for(i in 1:nrow(maze)){
        for(j in 1:ncol(maze)){
            expanded_maze[2*i - 1, 2*j - 1] <- maze[i,j]
        }
    }

    for (i in 1:nrow(maze)) {
        for (j in 1:ncol(maze)) {
            if (loop_mask[i, j]) {
              pipe <- maze[i,j]
              
              if (pipe %in% c('|', 'L', 'J','S')) {
                  expanded_maze[2*i-2,2*j-1] <- '|'
              }
              if (pipe %in% c('|', '7', 'F','S')) {
                expanded_maze[2*i,2*j-1] <- '|'
              }
              if (pipe %in% c('-', 'L', 'F','S')) {
                expanded_maze[2*i-1,2*j] <- '-'
              }
              if (pipe %in% c('-', 'J', '7','S')) {
                expanded_maze[2*i-1,2*j-2] <- '-'
              }  
            }
        }
    }

  
    loop_mask_expanded <- matrix(FALSE, nrow = nrow(expanded_maze), ncol = ncol(expanded_maze))

    for(i in 1:nrow(maze)){
        for(j in 1:ncol(maze)){
            loop_mask_expanded[2 * i - 1, 2 * j - 1] <- loop_mask[i,j]
        }
    }
  
    for (i in 1:nrow(maze)) {
      for (j in 1:ncol(maze)) {
        if (loop_mask[i, j]) {
          pipe <- maze[i,j]
          
          if (pipe %in% c('|', 'L', 'J','S')) {
            loop_mask_expanded[2*i-2,2*j-1] <- TRUE
          }
          if (pipe %in% c('|', '7', 'F','S')) {
            loop_mask_expanded[2*i,2*j-1] <- TRUE
          }
          if (pipe %in% c('-', 'L', 'F','S')) {
            loop_mask_expanded[2*i-1,2*j] <- TRUE
          }
          if (pipe %in% c('-', 'J', '7','S')) {
            loop_mask_expanded[2*i-1,2*j-2] <- TRUE
          }  
        }
      }
    }


    outside <- matrix(FALSE, nrow=nrow(expanded_maze), ncol=ncol(expanded_maze))
    queue <- list()
    
    # Add border cells to the queue
    for(i in 1:nrow(expanded_maze)){
        if(!loop_mask_expanded[i,1]) {
            queue <- c(queue, list(c(i,1)))
            outside[i,1] <- TRUE
        }
        if(!loop_mask_expanded[i, ncol(expanded_maze)]) {
            queue <- c(queue, list(c(i,ncol(expanded_maze))))
            outside[i,ncol(expanded_maze)] <- TRUE
        }
    }
    for(j in 1:ncol(expanded_maze)){
      if(!loop_mask_expanded[1, j]) {
            queue <- c(queue, list(c(1,j)))
            outside[1,j] <- TRUE
        }
        if(!loop_mask_expanded[nrow(expanded_maze), j]) {
            queue <- c(queue, list(c(nrow(expanded_maze),j)))
            outside[nrow(expanded_maze),j] <- TRUE
        }   
    }

    # BFS to find all outside cells
      while (length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]

        row <- current[1]
        col <- current[2]

        # Check neighbors
        neighbors <- list(c(row-1,col),c(row+1,col), c(row,col-1), c(row,col+1))
        for(neighbor in neighbors){
            n_row <- neighbor[1]
            n_col <- neighbor[2]

            if(n_row >=1 && n_row <= nrow(expanded_maze) && n_col >= 1 && n_col <= ncol(expanded_maze)){
                if(!loop_mask_expanded[n_row,n_col] && !outside[n_row,n_col]){
                    outside[n_row,n_col] <- TRUE
                    queue <- c(queue, list(c(n_row,n_col)))
                }
            }
        }
      }

  
    # Count enclosed tiles in the original maze
    enclosed_count <- 0
    for (i in 1:nrow(maze)) {
        for (j in 1:ncol(maze)) {
            if (!loop_mask[i, j] && !outside[2*i-1,2*j-1]) {
                enclosed_count <- enclosed_count + 1
            }
        }
    }    
    return(enclosed_count)
}

# Main function
main <- function() {
    # Read the maze from input.txt
    maze <- readLines("input.txt")
    maze <- do.call(rbind, strsplit(maze, ""))
    
    # Part 1: Calculate farthest distance
    result_part1 <- calculate_farthest_distance(maze)
    cat("Part 1: Farthest distance:", result_part1$max_dist, "\n")
    
    # Part 2: Calculate enclosed tiles
    enclosed_tiles <- calculate_enclosed_tiles(maze,result_part1$visited)
    cat("Part 2: Enclosed tiles:", enclosed_tiles, "\n")
}

# Run the main function
main()
