
# Function to parse a brick from a string
parse_brick <- function(line) {
  ends <- strsplit(line, "~")[[1]]
  start <- as.integer(strsplit(ends[1], ",")[[1]])
  end <- as.integer(strsplit(ends[2], ",")[[1]])
  list(start = start, end = end)
}

# Function to check if two bricks overlap in x-y plane
overlap <- function(brick1, brick2) {
  max_start_x <- max(brick1$start[1], brick2$start[1])
  min_end_x <- min(brick1$end[1], brick2$end[1])
  max_start_y <- max(brick1$start[2], brick2$start[2])
  min_end_y <- min(brick1$end[2], brick2$end[2])
  
  max_start_x <= min_end_x && max_start_y <= min_end_y
}

# Function to drop bricks
drop_bricks <- function(bricks) {
  # Sort bricks by their lowest z coordinate
  bricks <- bricks[order(sapply(bricks, function(b) min(b$start[3], b$end[3])))]
  
  # Store height map and brick positions
  height_map <- matrix(0, nrow = 10, ncol = 10)
  brick_positions <- list()

  # Drop each brick
  for (i in seq_along(bricks)) {
    brick <- bricks[[i]]
    max_height <- 0
    
    # Find the maximum height this brick can fall to
        for (x in brick$start[1]:brick$end[1]) {
          for (y in brick$start[2]:brick$end[2]) {
            max_height <- max(max_height, height_map[x + 1, y + 1])
          }
        }

    # Calculate fall distance and update brick's z coordinates
    fall_distance <- brick$start[3] - (max_height + 1)
    brick$start[3] <- brick$start[3] - fall_distance
    brick$end[3] <- brick$end[3] - fall_distance
    
    #Update height_map with new height of the settled brick
        for (x in brick$start[1]:brick$end[1]) {
          for (y in brick$start[2]:brick$end[2]) {
              height_map[x+1,y+1] = brick$end[3]
          }
        }

    bricks[[i]] <- brick  #update the brick in the list
  }
  
  bricks
}


# Function to find supported bricks
find_supports <- function(bricks) {
    supports <- vector("list", length(bricks))
    supported_by <- vector("list", length(bricks))

    for (i in seq_along(bricks)) {
      for (j in seq_along(bricks)) {
          if (i != j) {
            if (overlap(bricks[[i]], bricks[[j]]) && bricks[[i]]$end[3] + 1 == bricks[[j]]$start[3]) {
                supports[[i]] <- c(supports[[i]], j)
                supported_by[[j]] <- c(supported_by[[j]], i)
            }
          }
        }
    }

    list(supports = supports, supported_by = supported_by)
}

# Function to calculate the number of bricks that would fall if one is removed
count_falling_bricks <- function(bricks, supports, supported_by) {
  
  safe_count <- 0
  total_fall_count <- 0

  for (i in seq_along(bricks)) {
    would_fall <- rep(FALSE, length(bricks))
    would_fall[i] <- TRUE
    
    q <- c(i)
    while(length(q) > 0) {
      curr <- q[1]
      q <- q[-1]
      
      for(supported_brick in supports[[curr]]) {
        if(!would_fall[supported_brick]) {
          all_supporting_gone <- TRUE
          for(supporter in supported_by[[supported_brick]]) {
            if(!would_fall[supporter]) {
              all_supporting_gone <- FALSE
              break
            }
          }
          if(all_supporting_gone) {
            would_fall[supported_brick] <- TRUE
            q <- c(q, supported_brick)
          }
        }
      }
    }
    
    fall_count <- sum(would_fall) - 1
    
    if (fall_count == 0){
        safe_count <- safe_count + 1
    }
    total_fall_count = total_fall_count + fall_count
    
  }
  
  list(safe_count = safe_count, total_fall_count = total_fall_count)
}


# Main function
main <- function() {
  # Read input from file
  lines <- readLines("input.txt")
  
  # Parse bricks
  bricks <- lapply(lines, parse_brick)
  
  # Drop bricks
  bricks <- drop_bricks(bricks)
  
    # Find supports and supported_by relationships after settling
  support_data <- find_supports(bricks)
  supports <- support_data$supports
  supported_by <- support_data$supported_by


  # Calculate falling bricks
  result <- count_falling_bricks(bricks, supports, supported_by)
  
  # Output result
  cat("Part 1:", result$safe_count, "\n")       # Part 1
  cat("Part 2:", result$total_fall_count, "\n") # Part 2
}

# Run main function
main()
