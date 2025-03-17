
# Function to convert a 3x3 neighborhood to a decimal index
neighborhood_to_index <- function(image, row, col, default_val) {
  index_str <- ""
  for (i in (row - 1):(row + 1)) {
    for (j in (col - 1):(col + 1)) {
      if (i >= 1 && i <= nrow(image) && j >= 1 && j <= ncol(image)) {
        index_str <- paste0(index_str, ifelse(image[i, j] == "#", "1", "0"))
      } else {
        index_str <- paste0(index_str, ifelse(default_val == "#", "1", "0"))
      }
    }
  }
  return(strtoi(index_str, base = 2))
}

# Function to enhance the image
enhance_image <- function(image, algorithm, default_val) {
  new_rows <- nrow(image) + 2
  new_cols <- ncol(image) + 2
  enhanced_image <- matrix(default_val, nrow = new_rows, ncol = new_cols)
  
  next_default_val <- ifelse(default_val == ".", substr(algorithm, 1, 1), substr(algorithm, nchar(algorithm), nchar(algorithm)))

  for (i in 1:new_rows) {
    for (j in 1:new_cols) {
      index <- neighborhood_to_index(image, i - 1, j - 1, default_val) + 1
      enhanced_image[i, j] <- substr(algorithm, index, index)
    }
  }
  return(list(image = enhanced_image, default_val = next_default_val))
}


# Main function
main <- function() {
  # Read input from file
  input <- readLines("input.txt")
  
  # Separate algorithm and image
  algorithm <- input[1]
  image_data <- input[3:length(input)]
  
  # Convert image to matrix
  image <- do.call(rbind, strsplit(image_data, ""))
  
  # Part 1: Enhance twice
  default_val <- "."
  enhanced_result <- list(image = image, default_val = default_val)
  for (i in 1:2) {
      enhanced_result <- enhance_image(enhanced_result$image, algorithm, enhanced_result$default_val)
  }
  part1_answer <- sum(enhanced_result$image == "#")
  cat("Part 1:", part1_answer, "\n")

  # Part 2: Enhance 50 times
    enhanced_result <- list(image = image, default_val = default_val)
  for (i in 1:50) {
      enhanced_result <- enhance_image(enhanced_result$image, algorithm, enhanced_result$default_val)
  }

  part2_answer <- sum(enhanced_result$image == "#")
  cat("Part 2:", part2_answer, "\n")
}

# Run the main function
main()
