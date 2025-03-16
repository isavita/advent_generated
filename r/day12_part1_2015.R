
# Function to extract and sum numbers from a JSON-like string
sum_numbers_in_json <- function(input_string) {
  # Use gsub to remove everything except numbers and minus signs, then split by that pattern.
  #  The regular expression '[^0-9-]+' matches any character that ISN'T (^)
  #  a digit (0-9) or a hyphen (-).  The '+' after [^0-9-]+ means "one or
  #  more" of these non-digit/non-hyphen characters.  We replace all such
  #  sequences with a single space. This effectively isolates the numbers.
  numbers_string <- gsub("[^0-9-]+", " ", input_string)

  # Split the string by spaces to get individual numbers (as strings)
  number_strings <- unlist(strsplit(numbers_string, " +"))

  # Convert to numeric, handling empty strings (from leading/trailing spaces),
  #  and NAs (from consecutive spaces or non-numeric characters that got
  # through).  sum() will ignore NAs by default.
  numbers <- as.numeric(number_strings)
  numbers <- numbers[!is.na(numbers)] # Remove NAs explicitly

  # Return the sum
  return(sum(numbers))
}



# Main function to read input and print the sum
main <- function() {
  # Read the entire file content as a single string
  file_content <- readChar("input.txt", file.info("input.txt")$size)

  # Calculate the sum of the numbers
  total_sum <- sum_numbers_in_json(file_content)

  # Print the total sum to standard output
  cat(total_sum, "\n")
}

# Run the main function
if (sys.nframe() == 0) {
  main()
}
