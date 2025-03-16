
# Function to calculate the points for a single scratchcard
calculate_points <- function(winning_numbers, my_numbers) {
  matches <- sum(my_numbers %in% winning_numbers)
  if (matches > 0) {
    return(2^(matches - 1))
  } else {
    return(0)
  }
}

# Main function to process the input file and calculate the total points
main <- function() {
  # Read the input file
  lines <- readLines("input.txt")
  
  total_points <- 0
  
  # Process each line (scratchcard)
  for (line in lines) {
    # Split the line into the card part and the numbers part
    parts <- strsplit(line, ": ")[[1]]
    numbers_part <- parts[2]
    
    # Split the numbers part into winning numbers and my numbers
    number_sets <- strsplit(numbers_part, " \\| ")[[1]]
    winning_numbers <- as.integer(strsplit(number_sets[1], " +")[[1]])
    my_numbers <- as.integer(strsplit(number_sets[2], " +")[[1]])
    
    # Remove NA values that result from parsing empty strings
    winning_numbers <- winning_numbers[!is.na(winning_numbers)]
    my_numbers <- my_numbers[!is.na(my_numbers)]
    
    # Calculate points for the current card and add to the total
    total_points <- total_points + calculate_points(winning_numbers, my_numbers)
  }
  
  # Print the total points
  cat(total_points, "\n")
}

# Run the main function
main()
