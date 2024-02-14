
instructions <- as.numeric(readLines("input.txt"))

position <- 1
steps <- 0

while(position >= 1 && position <= length(instructions)) {
  jump <- instructions[position]
  instructions[position] <- ifelse(jump >= 3, jump - 1, jump + 1)
  position <- position + jump
  steps <- steps + 1
}

print(steps)
