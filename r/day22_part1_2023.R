
# Function to parse a single brick's coordinates from a string
parse_brick <- function(line) {
  parts <- unlist(strsplit(line, "~"))
  start_coords <- as.integer(unlist(strsplit(parts[1], ",")))
  end_coords <- as.integer(unlist(strsplit(parts[2], ",")))
  return(list(
    x = start_coords[1]:end_coords[1],
    y = start_coords[2]:end_coords[2],
    z = start_coords[3]:end_coords[3]
  ))
}

# Function to determine if two bricks overlap in x and y coordinates
overlap_xy <- function(brick1, brick2) {
  x_overlap <- any(brick1$x %in% brick2$x)
  y_overlap <- any(brick1$y %in% brick2$y)
  return(x_overlap && y_overlap)
}

# Function to simulate bricks falling and settling
settle_bricks <- function(bricks) {
  # Sort bricks by their lowest z-coordinate
  bricks <- bricks[order(sapply(bricks, function(b) min(b$z)))]

  # Keep track of the settled bricks
  settled <- list()
  
  # Iterate to drop each brick to settled position
  for (brick in bricks) {
    max_z <- 1  # Default to ground level

    # Iterate through all settled bricks, to potentially land on top of one
    for (settled_brick in settled) {
      if (overlap_xy(brick, settled_brick)) {
        max_z <- max(max_z, max(settled_brick$z) + 1)
      }
    }
    
    # Drop the current brick based on the determined level
    drop_distance <- min(brick$z) - max_z
    brick$z <- brick$z - drop_distance

    settled[[length(settled) + 1]] <- brick
  }

  return(settled)
}

# Function to determine which bricks support other bricks
find_supports <- function(bricks) {
    supports <- vector("list", length(bricks))
    supported_by <- vector("list", length(bricks))

    # Iterate through all brick pairs to check for support
    for (i in seq_along(bricks)) {
        for (j in seq_along(bricks)) {
            if (i != j) {
                # Does brick i sit on top of brick j?
                if (overlap_xy(bricks[[i]], bricks[[j]]) && min(bricks[[i]]$z) == max(bricks[[j]]$z) + 1) {
                    supports[[j]] <- c(supports[[j]], i)  # j supports i
                    supported_by[[i]] <- c(supported_by[[i]], j)  # i is supported by j
                }
            }
        }
    }
    return(list(supports = supports, supported_by = supported_by))
}

# Function to count safe-to-disintegrate bricks
count_safe_bricks <- function(bricks, supports, supported_by) {
  safe_count <- 0
  for (i in seq_along(bricks)) {
    can_disintegrate <- TRUE
    for (supported_brick_index in supports[[i]]) {
      if (length(supported_by[[supported_brick_index]]) == 1) {
        can_disintegrate <- FALSE
        break
      }
    }
    if (can_disintegrate) {
      safe_count <- safe_count + 1
    }
  }
  return(safe_count)
}

# Main function to read input, process, and print output
main <- function() {
  # Read input from file
  input_lines <- readLines("input.txt")

  # Parse bricks from input lines
  bricks <- lapply(input_lines, parse_brick)

  # Simulate bricks settling
  settled_bricks <- settle_bricks(bricks)

  # Find support relationships
  support_data <- find_supports(settled_bricks)
    
  # Count safe-to-disintegrate bricks
  safe_bricks_count <- count_safe_bricks(settled_bricks, support_data$supports, support_data$supported_by)

  # Print the result
  cat(safe_bricks_count, "\n")
}

# Run the main function
main()
