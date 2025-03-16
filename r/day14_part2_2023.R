
# Function to tilt the platform to the north
tilt_north <- function(platform) {
  num_rows <- nrow(platform)
  num_cols <- ncol(platform)
  
  for (col in 1:num_cols) {
    next_open <- 1
    for (row in 1:num_rows) {
      if (platform[row, col] == "O") {
        platform[row, col] <- "."
        platform[next_open, col] <- "O"
        next_open <- next_open + 1
      } else if (platform[row, col] == "#") {
        next_open <- row + 1
      }
    }
  }
  return(platform)
}

# Function to calculate the total load
calculate_load <- function(platform) {
  num_rows <- nrow(platform)
  load <- 0
  for (row in 1:num_rows) {
    load <- load + sum(platform[row, ] == "O") * (num_rows - row + 1)
  }
  return(load)
}

# Function to perform a single spin cycle (North, West, South, East)
spin_cycle <- function(platform) {
    
    for(dir in 1:4){
        platform <- tilt_north(platform)
        platform <- t(apply(platform, 2, rev)) # Rotate 90 degrees clockwise
    }

  return(platform)
}


# Main function
main <- function() {
  # Read the input from the file
  input_data <- readLines("input.txt")
  
  # Convert the input to a matrix
  platform <- do.call(rbind, strsplit(input_data, ""))
  
  # Part 1: Tilt north and calculate load
  tilted_platform <- tilt_north(platform)
  part1_load <- calculate_load(tilted_platform)
  cat("Part 1: Total load on north support beams:", part1_load, "\n")
  
  # Part 2: Run spin cycles and calculate load
  platform <- do.call(rbind, strsplit(input_data, "")) # Reset platform
  cycles <- 1000000000
  
  seen_states <- list()
  cycle_loads <- numeric()
  
  for (i in 1:cycles) {
    platform <- spin_cycle(platform)
    state <- paste(as.vector(platform), collapse = "")
    
    if (state %in% names(seen_states)) {
      cycle_start <- seen_states[[state]]
      cycle_length <- i - cycle_start
      remaining_cycles <- cycles - i
      final_cycle_index <- cycle_start + (remaining_cycles %% cycle_length)
      part2_load <- cycle_loads[final_cycle_index]
      break
    } else {
      seen_states[[state]] <- i
      cycle_loads[i] <- calculate_load(platform)
    }
  }
    
  
  cat("Part 2: Total load after", cycles, "cycles:", part2_load, "\n")
}

# Run the main function
main()
