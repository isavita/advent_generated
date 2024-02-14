
# Read input from file
input <- readLines("input.txt")

# Function to count trees encountered for a given slope
count_trees <- function(right, down) {
  trees <- 0
  x <- 1
  y <- 1
  width <- nchar(input[1])
  height <- length(input)
  
  while (y <= height) {
    if (substr(input[y], x, x) == "#") {
      trees <- trees + 1
    }
    x <- (x + right) %% width
    if (x == 0) {
      x <- width
    }
    y <- y + down
  }
  
  return(trees)
}

# Calculate the number of trees encountered for each slope
result <- count_trees(1, 1) * count_trees(3, 1) * count_trees(5, 1) * count_trees(7, 1) * count_trees(1, 2)

# Print the result
print(result)
