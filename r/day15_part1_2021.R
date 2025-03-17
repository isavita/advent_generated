
# Function to find the lowest total risk path using Dijkstra's algorithm
find_lowest_risk <- function(risk_map) {
  rows <- nrow(risk_map)
  cols <- ncol(risk_map)
  
  # Initialize distances with Inf, except for the starting point
  distances <- matrix(Inf, nrow = rows, ncol = cols)
  distances[1, 1] <- 0
  
  # Priority queue (implemented as a list for simplicity and efficiency in this case)
  queue <- list(c(1, 1))  # (row, col)
  
  # Directions: down, right, up, left
  dr <- c(1, 0, -1, 0)
  dc <- c(0, 1, 0, -1)
  
  while (length(queue) > 0) {
    # Find the node with the minimum distance in the queue (most efficient way for this problem)
    min_dist <- Inf
    min_index <- -1
    
    for (i in seq_along(queue)) {
        current_row <- queue[[i]][1]
        current_col <- queue[[i]][2]
      if (distances[current_row, current_col] < min_dist) {
        min_dist <- distances[current_row, current_col]
        min_index <- i
      }
    }
    
    if (min_index == -1) break  # No more reachable nodes

    # Extract current node (and remove from queue)
    current_node <- queue[[min_index]]
    queue <- queue[-min_index]
    current_row <- current_node[1]
    current_col <- current_node[2]

    # Explore neighbors
    for (i in 1:4) {
      new_row <- current_row + dr[i]
      new_col <- current_col + dc[i]
      
      # Check bounds
      if (new_row >= 1 && new_row <= rows && new_col >= 1 && new_col <= cols) {
        new_dist <- distances[current_row, current_col] + risk_map[new_row, new_col]
        
        # If shorter path found, update distance and add to queue (or update if already in queue)
        if (new_dist < distances[new_row, new_col]) {
          distances[new_row, new_col] <- new_dist
          
          # Add to queue if not already present
          found <- FALSE
          for (j in seq_along(queue)) {
            if (all(queue[[j]] == c(new_row, new_col))) {
                found = TRUE
                break;
            }
          }
          if(!found){
            queue <- c(queue, list(c(new_row, new_col)))  
          }
        }
      }
    }
  }
  
  return(distances[rows, cols])
}

# Main function
main <- function() {
  # Read input from file
  input_data <- readLines("input.txt")
  
  # Convert input to a numeric matrix
  risk_map <- do.call(rbind, lapply(strsplit(input_data, ""), as.numeric))
  
  # Find the lowest total risk
  lowest_risk <- find_lowest_risk(risk_map)
  
  # Print the result
  cat(lowest_risk, "\n")
}

# Execute the main function
main()
