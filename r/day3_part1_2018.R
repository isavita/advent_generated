# Read the input file
claims <- readLines("input.txt")

# Initialize a fabric grid of size 1000x1000
fabric <- matrix(0, nrow = 1000, ncol = 1000)

# Process each claim
for (claim in claims) {
  # Extract the coordinates and dimensions using regular expressions
  matches <- regmatches(claim, regexec("#\\d+ @ (\\d+),(\\d+): (\\d+)x(\\d+)", claim))
  x <- as.integer(matches[[1]][2])
  y <- as.integer(matches[[1]][3])
  width <- as.integer(matches[[1]][4])
  height <- as.integer(matches[[1]][5])
  
  # Mark the fabric grid with the claim ID (or just increment the count)
  fabric[(y + 1):(y + height), (x + 1):(x + width)] <- fabric[(y + 1):(y + height), (x + 1):(x + width)] + 1
}

# Count the number of square inches with two or more claims
overlap_count <- sum(fabric >= 2)

# Print the result
cat(overlap_count, "\n")