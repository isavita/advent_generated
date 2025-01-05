
# Initialize a 1000x1000 matrix of lights, all off (0)
lights <- matrix(0, nrow = 1000, ncol = 1000)

# Function to process a single instruction
process_instruction <- function(instruction) {
  parts <- strsplit(instruction, " ")[[1]]
  
  if (parts[1] == "turn") {
    action <- parts[2]
    start_coords <- as.numeric(strsplit(parts[3], ",")[[1]]) + 1
    end_coords <- as.numeric(strsplit(parts[5], ",")[[1]]) + 1
  } else { # toggle
    action <- "toggle"
    start_coords <- as.numeric(strsplit(parts[2], ",")[[1]]) + 1
    end_coords <- as.numeric(strsplit(parts[4], ",")[[1]]) + 1
  }
  
  for (row in start_coords[1]:end_coords[1]) {
    for (col in start_coords[2]:end_coords[2]) {
      if (action == "on") {
        lights[row, col] <<- 1
      } else if (action == "off") {
        lights[row, col] <<- 0
      } else { # toggle
        lights[row, col] <<- 1 - lights[row, col]
      }
    }
  }
}

# Read instructions from input.txt
instructions <- readLines("input.txt")

# Process each instruction
for (instruction in instructions) {
  process_instruction(instruction)
}

# Calculate and print the number of lit lights
lit_lights <- sum(lights)
cat(lit_lights, "\n")
