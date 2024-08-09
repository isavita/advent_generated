# Read input from the file
lines <- readLines("input.txt")

# Split lines into a matrix for easier column access
data_matrix <- strsplit(lines, split = "")[[1]]
data_matrix <- do.call(rbind, strsplit(lines, split = ""))

# Function to get the most and least common characters
get_messages <- function(data) {
  most_common <- apply(data, 2, function(col) {
    names(sort(table(col), decreasing = TRUE))[1]
  })
  
  least_common <- apply(data, 2, function(col) {
    names(sort(table(col)))[1]
  })
  
  list(most_common = paste(most_common, collapse = ""), least_common = paste(least_common, collapse = ""))
}

# Get the messages
messages <- get_messages(data_matrix)

# Print the results
cat("Most common message:", messages$most_common, "\n")
cat("Least common message:", messages$least_common, "\n")