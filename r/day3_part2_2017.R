# Read the target value from the file
target <- as.integer(readLines("input.txt"))

# Initialize the grid
grid <- matrix(0, nrow = 1001, ncol = 1001)
grid[501, 501] <- 1

# Initialize the coordinates and directions
x <- 0
y <- 0
dx <- 0
dy <- -1

# Initialize the value
value <- 1

# Loop until the target value is reached
while(TRUE) {
  # Check for the spiral rules
  if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1-y)) {
    temp <- dx
    dx <- -dy
    dy <- temp
  }
  
  # Move to the next position
  x <- x + dx
  y <- y + dy
  
  # Calculate the value at the new position
  value <- 0
  for (dx2 in -1:1) {
    for (dy2 in -1:1) {
      if (x + dx2 + 501 > 0 && x + dx2 + 501 <= 1001 && y + dy2 + 501 > 0 && y + dy2 + 501 <= 1001) {
        value <- value + grid[x + dx2 + 501, y + dy2 + 501]
      }
    }
  }
  
  # Update the grid
  grid[x + 501, y + 501] <- value
  
  # Check if the target value is reached
  if (value > target) {
    print(value)
    break
  }
}