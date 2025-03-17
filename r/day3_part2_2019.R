
# Function to calculate the path of a wire
calculate_path <- function(wire_path) {
  x <- 0
  y <- 0
  steps <- 0
  path <- list()
  
  moves <- strsplit(wire_path, ",")[[1]]
  
  for (move in moves) {
    direction <- substr(move, 1, 1)
    distance <- as.integer(substr(move, 2, nchar(move)))
    
    for (i in 1:distance) {
      steps <- steps + 1
      if (direction == "R") {
        x <- x + 1
      } else if (direction == "L") {
        x <- x - 1
      } else if (direction == "U") {
        y <- y + 1
      } else if (direction == "D") {
        y <- y - 1
      }
      path[[paste(x, y, sep=",")]] <- steps
    }
  }
  return(path)
}

# Function to find intersections and calculate Manhattan distance or combined steps
find_intersections <- function(path1, path2, part2 = FALSE) {
    
  intersections <- intersect(names(path1), names(path2))
  
  if (length(intersections) == 0) {
    return(Inf) 
  }
  
  if (!part2) {
    # Part 1: Manhattan Distance
    min_distance <- Inf
    for (intersection in intersections) {
      coords <- as.integer(strsplit(intersection, ",")[[1]])
      distance <- abs(coords[1]) + abs(coords[2])
      min_distance <- min(min_distance, distance)
    }
    return(min_distance)
  } else {
    # Part 2: Combined Steps
    min_steps <- Inf
    for (intersection in intersections) {
      combined_steps <- path1[[intersection]] + path2[[intersection]]
      min_steps <- min(min_steps, combined_steps)
    }
    return(min_steps)
  }
}

# Main function
main <- function() {
  # Read input from file
  input <- readLines("input.txt")
  wire1_path_str <- input[1]
  wire2_path_str <- input[2]
  
  # Calculate paths for both wires
  wire1_path <- calculate_path(wire1_path_str)
  wire2_path <- calculate_path(wire2_path_str)
  
  # Part 1: Find closest intersection by Manhattan distance
  min_manhattan_distance <- find_intersections(wire1_path, wire2_path)
  cat("Part 1: Minimum Manhattan Distance:", min_manhattan_distance, "\n")
  
  # Part 2: Find closest intersection by combined steps
  min_combined_steps <- find_intersections(wire1_path, wire2_path, part2 = TRUE)
  cat("Part 2: Minimum Combined Steps:", min_combined_steps, "\n")
}

# Run the main function
if (sys.nframe() == 0) {
  main()
}
