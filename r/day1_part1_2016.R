# Read the input from the file
instructions <- readLines("input.txt")

# Initialize starting position and direction
position <- c(0, 0)
direction <- 0 # 0 = North, 1 = East, 2 = South, 3 = West

# Define the movement vectors for North, East, South, West
movement <- rbind(c(0, 1), c(1, 0), c(0, -1), c(-1, 0))

# Process each instruction
for (instruction in unlist(strsplit(instructions, ", "))) {
  turn <- substr(instruction, 1, 1)
  steps <- as.integer(substr(instruction, 2, nchar(instruction)))
  
  # Update direction based on turn
  direction <- (direction + ifelse(turn == "R", 1, -1)) %% 4
  
  # Move forward in the current direction
  position <- position + steps * movement[direction + 1, ]
}

# Calculate Manhattan distance
distance <- sum(abs(position))
cat(distance, "\n")