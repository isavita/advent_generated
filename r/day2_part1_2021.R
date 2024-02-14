
instructions <- readLines("input.txt")
horizontal <- 0
depth <- 0

for (instruction in instructions) {
  parts <- strsplit(instruction, " ")[[1]]
  direction <- parts[1]
  value <- as.numeric(parts[2])
  
  if (direction == "forward") {
    horizontal <- horizontal + value
  } else if (direction == "down") {
    depth <- depth + value
  } else if (direction == "up") {
    depth <- depth - value
  }
}

result <- horizontal * depth
print(result)
