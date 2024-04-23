registers <- list(a = 0, b = 0, c = 0, d = 0)
instructions <- readLines("input.txt")

executeInstructions <- function(instructions, registers) {
  i <- 1
  while(i <= length(instructions)) {
    parts <- strsplit(instructions[i], "\\s+")[[1]]
    if(parts[1] == "cpy") {
      val <- if(grepl("^[0-9]+$", parts[2])) as.integer(parts[2]) else registers[[parts[2]]]
      registers[[parts[3]]] <- val
    } else if(parts[1] == "inc") {
      registers[[parts[2]]] <- registers[[parts[2]]] + 1
    } else if(parts[1] == "dec") {
      registers[[parts[2]]] <- registers[[parts[2]]] - 1
    } else if(parts[1] == "jnz") {
      val <- if(grepl("^[0-9]+$", parts[2])) as.integer(parts[2]) else registers[[parts[2]]]
      if(val != 0) {
        i <- i + as.integer(parts[3]) - 1
      }
    }
    i <- i + 1
  }
  return(registers)
}

registers <- executeInstructions(instructions, registers)
print(registers$a)