
# Function to transpose a matrix (for easier column-wise comparison)
transpose <- function(matrix) {
  t(matrix)
}

# Function to check for reflection at a given index
check_reflection <- function(matrix, index, is_horizontal, smudge_allowed = FALSE) {
    rows <- nrow(matrix)
    cols <- ncol(matrix)
    diff_count <- 0

    if (is_horizontal) {
        limit <- min(index, rows - index)
        for (i in 1:limit) {
            diff_count <- diff_count + sum(matrix[index - i + 1, ] != matrix[index + i, ])
            if(diff_count > 1) return(FALSE)
        }
    } else {
        limit <- min(index, cols - index)
        for (i in 1:limit) {
            diff_count <- diff_count + sum(matrix[, index - i + 1] != matrix[, index + i])
            if(diff_count > 1) return(FALSE)
        }
    }
    
    if(smudge_allowed) {
      return (diff_count == 1)
    } else{
      return(diff_count == 0)
    }
}


# Function to find reflection line in a matrix
find_reflection <- function(matrix, smudge_allowed = FALSE) {
  rows <- nrow(matrix)
  cols <- ncol(matrix)

  # Check for horizontal reflection
  for (i in 1:(rows - 1)) {
    if (check_reflection(matrix, i, TRUE, smudge_allowed)) {
      return(i * 100)
    }
  }

  # Check for vertical reflection
  for (i in 1:(cols - 1)) {
    if (check_reflection(matrix, i, FALSE, smudge_allowed)) {
      return(i)
    }
  }

  return(0) # No reflection found
}

# Main function
main <- function() {
  # Read input from file
  input <- readLines("input.txt")

  # Split input into patterns
  patterns <- list()
  current_pattern <- c()
  for (line in input) {
    if (line == "") {
      patterns[[length(patterns) + 1]] <- current_pattern
      current_pattern <- c()
    } else {
      current_pattern <- c(current_pattern, line)
    }
  }
  patterns[[length(patterns) + 1]] <- current_pattern # Add the last pattern

  # Process each pattern
  total_sum_part1 <- 0
  total_sum_part2 <- 0
  for (pattern in patterns) {
    # Convert pattern to matrix
    matrix_pattern <- do.call(rbind, strsplit(pattern, ""))

    # Part 1: Find original reflection
    total_sum_part1 <- total_sum_part1 + find_reflection(matrix_pattern)

    #Part 2: Find the reflection with smudge
    total_sum_part2 <- total_sum_part2 + find_reflection(matrix_pattern, TRUE)

  }

  # Print the results
  cat("Part 1:", total_sum_part1, "\n")
  cat("Part 2:", total_sum_part2, "\n")

}

# Run the main function
main()
