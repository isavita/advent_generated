# Read the input file
input <- readLines("input.txt")

# Check if the last line has a newline character
if (substr(input[length(input)], nchar(input[length(input)]), nchar(input[length(input)])) != "\n") {
  # If not, add a newline character
  input[length(input)] <- paste0(input[length(input)], "\n")
}

# Remove whitespace and newline characters
input <- gsub("\\s+", "", input)

# Calculate the halfway point
halfway <- nchar(input) / 2

# Initialize sum
total_sum <- 0

# Loop through the characters in the input string
for (i in 1:nchar(input)) {
  # Calculate the next position
  next_position <- (i + halfway - 1) %% nchar(input) + 1
  
  # Check if the characters match
  if (substr(input, i, i) == substr(input, next_position, next_position)) {
    # If they match, add the character's value to the sum
    total_sum <- total_sum + as.integer(substr(input, i, i))
  }
}

# Print the sum
print(total_sum)