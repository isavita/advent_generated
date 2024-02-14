
instructions <- readLines("input.txt")
visited <- rep(FALSE, length(instructions))
accumulator <- 0
index <- 1

while(!visited[index]) {
  visited[index] <- TRUE
  parts <- strsplit(instructions[index], " ")[[1]]
  operation <- parts[1]
  argument <- as.numeric(parts[2])
  
  if(operation == "acc") {
    accumulator <- accumulator + argument
    index <- index + 1
  } else if(operation == "jmp") {
    index <- index + argument
  } else {
    index <- index + 1
  }
}

print(accumulator)
