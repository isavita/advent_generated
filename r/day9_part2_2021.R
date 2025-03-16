
# Function to read the heightmap from a file
read_heightmap <- function(filename) {
  lines <- readLines(filename)
  heightmap <- strsplit(lines, "")
  heightmap <- lapply(heightmap, as.integer)
  return(do.call(rbind, heightmap))
}

# Function to find low points and calculate risk levels
find_low_points <- function(heightmap) {
    rows <- nrow(heightmap)
    cols <- ncol(heightmap)
    low_points <- list()
    risk_levels <- 0

    for (i in 1:rows) {
        for (j in 1:cols) {
            current <- heightmap[i, j]
            adjacent <- c()

            # Check up
            if (i > 1) {
                adjacent <- c(adjacent, heightmap[i - 1, j])
            }
            # Check down
            if (i < rows) {
                adjacent <- c(adjacent, heightmap[i + 1, j])
            }
            # Check left
            if (j > 1) {
                adjacent <- c(adjacent, heightmap[i, j - 1])
            }
            # Check right
            if (j < cols) {
                adjacent <- c(adjacent, heightmap[i, j + 1])
            }

            if (all(current < adjacent)) {
                low_points <- c(low_points, list(c(i, j)))
                risk_levels <- risk_levels + current + 1
            }
        }
    }

    return(list(low_points = low_points, risk_levels = risk_levels))
}


# Function to calculate basin size using Depth-First Search
calculate_basin_size <- function(heightmap, row, col, visited) {
  rows <- nrow(heightmap)
  cols <- ncol(heightmap)
  
  if (row < 1 || row > rows || col < 1 || col > cols || 
      heightmap[row, col] == 9 || paste(row, col) %in% visited) {
    return(list(size = 0, visited = visited))
  }
  
  visited <- c(visited, paste(row, col))
  size <- 1
  
  directions <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  
  for (dir in directions) {
    result <- calculate_basin_size(heightmap, row + dir[1], col + dir[2], visited)
    size <- size + result$size
    visited <- result$visited
  }
  
  return(list(size = size, visited = visited))
}


# Function to find the three largest basins
find_largest_basins <- function(heightmap, low_points) {
    basin_sizes <- c()
    
    for (point in low_points) {
        row <- point[1]
        col <- point[2]
        visited <- c()
      basin_size <- calculate_basin_size(heightmap, row, col, visited)$size
      basin_sizes <- c(basin_sizes, basin_size)
    }
    
    basin_sizes <- sort(basin_sizes, decreasing = TRUE)
    return(prod(basin_sizes[1:3]))
}


# Main function
main <- function() {
  # Read heightmap from file
  heightmap <- read_heightmap("input.txt")
  
  # Part 1: Find low points and calculate sum of risk levels
  result_part1 <- find_low_points(heightmap)
  low_points <- result_part1$low_points
  cat("Sum of risk levels:", result_part1$risk_levels, "\n")
  
  # Part 2: Find the three largest basins and multiply their sizes
  product_of_basins <- find_largest_basins(heightmap, low_points)
  cat("Product of the sizes of the three largest basins:", product_of_basins, "\n")
}

# Run the main function
main()
