
# Function to check if a point is a low point
is_low_point <- function(heightmap, row, col) {
  rows <- nrow(heightmap)
  cols <- ncol(heightmap)
  current_height <- heightmap[row, col]
  
  # Check adjacent locations (up, down, left, right)
  if (row > 1 && heightmap[row - 1, col] <= current_height) return(FALSE)
  if (row < rows && heightmap[row + 1, col] <= current_height) return(FALSE)
  if (col > 1 && heightmap[row, col - 1] <= current_height) return(FALSE)
  if (col < cols && heightmap[row, col + 1] <= current_height) return(FALSE)
  
  return(TRUE)
}

# Main function
main <- function() {
  # Read the heightmap from input.txt
  heightmap_lines <- readLines("input.txt")
  heightmap <- do.call(rbind, lapply(strsplit(heightmap_lines, ""), as.integer))
  
  rows <- nrow(heightmap)
  cols <- ncol(heightmap)
  
  total_risk_level <- 0
  
  # Iterate through the heightmap to find low points
  for (row in 1:rows) {
    for (col in 1:cols) {
      if (is_low_point(heightmap, row, col)) {
        risk_level <- heightmap[row, col] + 1
        total_risk_level <- total_risk_level + risk_level
      }
    }
  }
  
  # Print the sum of the risk levels
  cat(total_risk_level, "\n")
}

# Run the main function
main()
