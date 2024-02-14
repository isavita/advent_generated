
# Read input file
input <- readLines("input.txt")
instructions <- data.frame(instruction = input, executed = FALSE)

# Function to execute each instruction
execute_instruction <- function(instruction) {
  operation <- substr(instruction, 1, 3)
  argument <- as.numeric(substr(instruction, 5, nchar(instruction)))
  
  if (operation == "acc") {
    accumulator <<- accumulator + argument
    return(1)
  } else if (operation == "jmp") {
    return(argument)
  } else {
    return(1)
  }
}

# Part One
accumulator <- 0
index <- 1

while (index <= nrow(instructions)) {
  if (instructions$executed[index]) {
    break
  }
  
  instructions$executed[index] <- TRUE
  index <- index + execute_instruction(instructions$instruction[index])
}

print(paste("Part One - Accumulator Value:", accumulator))

# Part Two
for (i in 1:nrow(instructions)) {
  instructions <- data.frame(instruction = input, executed = FALSE)
  accumulator <- 0
  index <- 1
  changed <- FALSE
  
  if (substr(instructions$instruction[i], 1, 3) == "jmp") {
    instructions$instruction[i] <- gsub("jmp", "nop", instructions$instruction[i])
    changed <- TRUE
  } else if (substr(instructions$instruction[i], 1, 3) == "nop") {
    instructions$instruction[i] <- gsub("nop", "jmp", instructions$instruction[i])
    changed <- TRUE
  }
  
  if (changed) {
    while (index <= nrow(instructions)) {
      if (instructions$executed[index]) {
        break
      }
      
      instructions$executed[index] <- TRUE
      index <- index + execute_instruction(instructions$instruction[index])
    }
    
    if (index > nrow(instructions)) {
      print(paste("Part Two - Accumulator Value:", accumulator))
      break
    }
  }
}
