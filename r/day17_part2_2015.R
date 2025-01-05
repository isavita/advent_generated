
# Function to read container sizes from input.txt
read_containers <- function(filename = "input.txt") {
  as.integer(readLines(filename))
}

# Function to find all combinations of containers that sum to the target volume
find_combinations <- function(containers, target, current_combination = c(), index = 1, all_combinations = list()) {
  if (sum(current_combination) == target) {
    all_combinations[[length(all_combinations) + 1]] <- current_combination
    return(all_combinations)
  }
  if (sum(current_combination) > target || index > length(containers)) {
    return(all_combinations)
  }

  # Include the current container
  all_combinations <- find_combinations(containers, target, c(current_combination, containers[index]), index + 1, all_combinations)
  # Exclude the current container
  all_combinations <- find_combinations(containers, target, current_combination, index + 1, all_combinations)
  
  return(all_combinations)
}

# Main function to solve the puzzle
solve_puzzle <- function(filename = "input.txt", target_volume = 150) {
  containers <- read_containers(filename)
  all_combinations <- find_combinations(containers, target_volume)
  
  # Part 1: Count all combinations
  part1_answer <- length(all_combinations)
  
  # Part 2: Find minimum number of containers and count combinations with that number
  if (length(all_combinations) > 0) {
    min_containers <- min(sapply(all_combinations, length))
    part2_answer <- sum(sapply(all_combinations, length) == min_containers)
  } else {
    min_containers <- 0
    part2_answer <- 0
  }
  
  cat("Part 1:", part1_answer, "\n")
  cat("Part 2:", part2_answer, "\n")
}

# Run the solution
solve_puzzle()
