
# Read input from file
input <- readLines("input.txt")

# Calculate total square feet of wrapping paper needed
total_paper <- 0
for (i in 1:length(input)) {
  dimensions <- as.numeric(strsplit(input[i], "x")[[1]])
  l <- dimensions[1]
  w <- dimensions[2]
  h <- dimensions[3]
  
  side1 <- l * w
  side2 <- w * h
  side3 <- h * l
  
  extra_paper <- min(side1, side2, side3)
  
  total_paper <- total_paper + 2*side1 + 2*side2 + 2*side3 + extra_paper
}

# Print the total square feet of wrapping paper needed
print(total_paper)
