
# Function to read the grid from a file
read_grid <- function(filename) {
  lines <- readLines(filename)
  grid <- do.call(rbind, lapply(strsplit(lines, ""), function(x) x))
  return(grid)
}

# Function to find the starting position (S)
find_start <- function(grid) {
  start_pos <- which(grid == "S", arr.ind = TRUE)
  return(c(start_pos[1], start_pos[2]))
}

# Function to calculate reachable garden plots
solve <- function(filename, steps) {
    grid <- read_grid(filename)
    start <- find_start(grid)
    
    q <- matrix(c(start[1], start[2]), ncol = 2, byrow = TRUE) # Initialize queue with start
    visited <- matrix(FALSE, nrow = nrow(grid), ncol = ncol(grid))
    visited[start[1], start[2]] <- TRUE
    
    
    for (i in 1:steps) {
        next_q <- matrix(nrow = 0, ncol = 2)
        
        while (nrow(q) > 0) {
            curr <- q[1, , drop = FALSE]
            q <- q[-1, , drop = FALSE]
            
            # Explore neighbors (North, South, East, West)
            neighbors <- matrix(c(
                curr[1] - 1, curr[2],
                curr[1] + 1, curr[2],
                curr[1], curr[2] - 1,
                curr[1], curr[2] + 1
            ), ncol = 2, byrow = TRUE)
            
            for (j in 1:nrow(neighbors)) {
                neighbor <- neighbors[j, ]
                
                # Check bounds and if it's a garden plot
                if (neighbor[1] >= 1 && neighbor[1] <= nrow(grid) &&
                    neighbor[2] >= 1 && neighbor[2] <= ncol(grid) &&
                    grid[neighbor[1], neighbor[2]] != "#" )
                {
                  if (!visited[neighbor[1], neighbor[2]]) {
                    
                    next_q <- rbind(next_q, neighbor)
                    visited[neighbor[1], neighbor[2]] <- TRUE
                  }
                }
            }
        }
      
        q <- next_q
        visited[,] <- FALSE  # reset visited to include only newly reached for next loop
        if(nrow(q) > 0){
            for(r in 1:nrow(q)){
                visited[q[r,1],q[r,2]] <- TRUE
            }
        }
    }

  
    final_q <- matrix(nrow = 0, ncol = 2)
    visited <- matrix(FALSE, nrow=nrow(grid), ncol=ncol(grid))
    for(i in 1:nrow(q)){
        if(grid[q[i,1], q[i,2]] != "#"){
           if (!visited[q[i,1], q[i,2]]){
               final_q <- rbind(final_q, q[i,,drop=FALSE])
               visited[q[i,1], q[i,2]] = TRUE;
           }
       }
    }

    return(nrow(final_q))
}

# Main function
main <- function() {
  result <- solve("input.txt", 64)
  cat(result, "\n")
}

# Run the main function
main()
