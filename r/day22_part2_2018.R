
# Function to calculate geologic index and erosion level
calculate_levels <- function(depth, target_x, target_y, max_x, max_y) {
  geologic_index <- matrix(0, nrow = max_y + 1, ncol = max_x + 1)
  erosion_level <- matrix(0, nrow = max_y + 1, ncol = max_x + 1)
  
  for (y in 0:max_y) {
    for (x in 0:max_x) {
      if ((x == 0 && y == 0) || (x == target_x && y == target_y)) {
        geologic_index[y + 1, x + 1] <- 0
      } else if (y == 0) {
        geologic_index[y + 1, x + 1] <- x * 16807
      } else if (x == 0) {
        geologic_index[y + 1, x + 1] <- y * 48271
      } else {
        geologic_index[y + 1, x + 1] <- erosion_level[y + 1, x] * erosion_level[y, x + 1]
      }
      erosion_level[y + 1, x + 1] <- (geologic_index[y + 1, x + 1] + depth) %% 20183
    }
  }
  return(erosion_level)
}

# Function to calculate region type
get_region_type <- function(erosion_level) {
  return(erosion_level %% 3)
}

# Dijkstra's algorithm for shortest path
dijkstra <- function(depth, target_x, target_y) {
    max_x <- target_x + 50  # Explore a larger area, may require more depending on inputs.
    max_y <- target_y + 50
    
    erosion_level <- calculate_levels(depth, target_x, target_y, max_x, max_y)
    region_type <- get_region_type(erosion_level)
    
    tools <- c("torch", "climbing", "neither")
    
    dist <- array(Inf, dim = c(max_y + 1, max_x + 1, 3))  # [y, x, tool]
    dist[1, 1, 1] <- 0  # Start at 0,0 with torch (tool index 1)
    
    queue <- list()
    queue[[1]] <- c(0, 0, 1, 0) # y, x, tool_index, distance
    
    visited <- array(FALSE, dim = c(max_y+1, max_x +1, 3))

    
    while (length(queue) > 0) {
        #find minimum distance in queue
        min_dist <- Inf
        min_idx <- -1
        for(i in 1:length(queue)){
            if(queue[[i]][4] < min_dist){
                min_dist = queue[[i]][4]
                min_idx = i
            }
        }
        
        
        current <- queue[[min_idx]]
        queue <- queue[-min_idx]
        
        cy <- current[1]
        cx <- current[2]
        ctool <- current[3]
        cdist <- current[4]

        if(visited[cy+1, cx + 1, ctool]) next
        visited[cy + 1, cx + 1, ctool] <- TRUE

        if (cy == target_y && cx == target_x && ctool == 1) {
          return(cdist)
        }        
        
        # Change tools
        for (new_tool in 1:3) {
            if (new_tool != ctool) {
              
              valid_tools <- switch(region_type[cy + 1, cx + 1] + 1,
                                    c(1, 2),       # 0: Rocky (torch, climbing)
                                    c(2, 3),       # 1: Wet (climbing, neither)
                                    c(1, 3))       # 2: Narrow (torch, neither)
              
              if(new_tool %in% valid_tools){  
                new_dist <- cdist + 7
                if (new_dist < dist[cy + 1, cx + 1, new_tool]) {
                    dist[cy + 1, cx + 1, new_tool] <- new_dist
                    queue[[length(queue) + 1]] <- c(cy, cx, new_tool, new_dist)
                }
              }
            }
        }

        # Move to adjacent regions
        moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
        for (move in moves) {
            ny <- cy + move[1]
            nx <- cx + move[2]
            
            if (ny >= 0 && ny <= max_y && nx >= 0 && nx <= max_x) {
              
              current_valid_tools <- switch(region_type[cy + 1, cx + 1] + 1,
                                    c(1, 2),       # 0: Rocky (torch, climbing)
                                    c(2, 3),       # 1: Wet (climbing, neither)
                                    c(1, 3))       # 2: Narrow (torch, neither)
              next_valid_tools <- switch(region_type[ny + 1, nx + 1] + 1,
                                    c(1, 2),       # 0: Rocky (torch, climbing)
                                    c(2, 3),       # 1: Wet (climbing, neither)
                                    c(1, 3))       # 2: Narrow (torch, neither)
              
              if(ctool %in% intersect(current_valid_tools, next_valid_tools)){              
                new_dist <- cdist + 1
                if (new_dist < dist[ny + 1, nx + 1, ctool]) {
                  dist[ny + 1, nx + 1, ctool] <- new_dist
                  queue[[length(queue) + 1]] <- c(ny, nx, ctool, new_dist)
                }
              }
            }
        }
    }
    
    return(Inf) # Should not reach here in a well-formed maze

}



main <- function() {
  # Read input from file
  input <- readLines("input.txt")
  
  # Parse depth and target coordinates
  depth <- as.integer(gsub("depth: ", "", input[1]))
  target_coords <- as.integer(strsplit(gsub("target: ", "", input[2]), ",")[[1]])
  target_x <- target_coords[1]
  target_y <- target_coords[2]
  
  # --- Part 1 ---
  max_x <- target_x
  max_y <- target_y
  
  erosion_level <- calculate_levels(depth, target_x, target_y, max_x, max_y)
  region_type <- get_region_type(erosion_level)
  
  total_risk <- sum(region_type)
  cat("Total risk level:", total_risk, "\n")

  # --- Part 2 ---
  shortest_time <- dijkstra(depth, target_x, target_y)
  cat("Shortest time to reach target:", shortest_time, "\n")
  
}

# Set working directory to the script's location.  This is *essential* for
# reliable file reading.
if (interactive()) {
    # Use this code when running the script in RStudio.  If this is
    # copied to a `main` function, the program will *not* work correctly
    # when run with `Rscript`.
    this_file <- gsub("--file=", "", commandArgs()[grepl("--file", commandArgs())])
    if (length(this_file) > 0) {
      setwd(dirname(this_file))
    }
}
# Call the main function
main()

