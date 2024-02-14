
# Read input from file
input <- readLines("input.txt")

# Function to calculate checksum
calculate_checksum <- function(ids) {
  twos <- 0
  threes <- 0
  
  for (id in ids) {
    counts <- table(strsplit(id, "")[[1]])
    if (2 %in% counts) {
      twos <- twos + 1
    }
    if (3 %in% counts) {
      threes <- threes + 1
    }
  }
  
  return (twos * threes)
}

# Calculate checksum for input
result <- calculate_checksum(input)
print(result)
