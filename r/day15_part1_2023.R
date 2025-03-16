
# Function to implement the HASH algorithm
hash_algorithm <- function(s) {
  current_value <- 0
  for (i in 1:nchar(s)) {
    # Get ASCII code of the current character
    ascii_code <- utf8ToInt(substr(s, i, i))
    # Increase the current value by the ASCII code
    current_value <- current_value + ascii_code
    # Multiply the current value by 17
    current_value <- current_value * 17
    # Set current value to the remainder of dividing itself by 256
    current_value <- current_value %% 256
  }
  return(current_value)
}

# Main function to read input, process data, and print the result
main <- function() {
  # Read the input file
  input <- readLines("input.txt")
  
  # Split the input into individual steps (comma-separated)
  steps <- unlist(strsplit(input, ","))
  
  # Calculate the HASH value for each step and sum them
  total_sum <- 0
  for (step in steps) {
    total_sum <- total_sum + hash_algorithm(step)
  }
  
  # Print the total sum
  cat(total_sum, "\n")
}

# Run the main function
if (sys.nframe() == 0) {
    main()
}
