
# Function to run the Intcode program
run_intcode <- function(program, input_val) {
  memory <- program
  i <- 1
  relative_base <- 0
  output <- numeric()
  
  get_param <- function(mode, pos) {
    if (pos > length(memory)) {
      memory <<- c(memory, rep(0, pos - length(memory)))
    }
    
    if (mode == 0) {
      if (memory[pos] + 1 > length(memory)) {
        memory <<- c(memory, rep(0, memory[pos] + 1 - length(memory)))
      }
      return(memory[memory[pos] + 1])
    } else if (mode == 1) {
      return(memory[pos])
    } else if (mode == 2) {
      if (relative_base + memory[pos] + 1 > length(memory)) {
        memory <<- c(memory, rep(0, relative_base + memory[pos] + 1 - length(memory)))
      }
      return(memory[relative_base + memory[pos] + 1])
    }
  }
  
  set_param <- function(mode, pos, value) {
    if (pos > length(memory)) {
      memory <<- c(memory, rep(0, pos - length(memory)))
    }
    
    if (mode == 0) {
      if (memory[pos] + 1 > length(memory)) {
        memory <<- c(memory, rep(0, memory[pos] + 1 - length(memory)))
      }
      memory[memory[pos] + 1] <<- value
    } else if (mode == 2) {
      if (relative_base + memory[pos] + 1 > length(memory)) {
        memory <<- c(memory, rep(0, relative_base + memory[pos] + 1 - length(memory)))
      }
      memory[relative_base + memory[pos] + 1] <<- value
    }
  }
  
  while (TRUE) {
    opcode <- memory[i] %% 100
    mode1 <- (memory[i] %/% 100) %% 10
    mode2 <- (memory[i] %/% 1000) %% 10
    mode3 <- (memory[i] %/% 10000) %% 10
    
    if (opcode == 99) {
      break
    } else if (opcode == 1) {
      set_param(mode3, i + 3, get_param(mode1, i + 1) + get_param(mode2, i + 2))
      i <- i + 4
    } else if (opcode == 2) {
      set_param(mode3, i + 3, get_param(mode1, i + 1) * get_param(mode2, i + 2))
      i <- i + 4
    } else if (opcode == 3) {
      set_param(mode1, i + 1, input_val)
      i <- i + 2
    } else if (opcode == 4) {
      output <- c(output, get_param(mode1, i + 1))
      i <- i + 2
    } else if (opcode == 5) {
      if (get_param(mode1, i + 1) != 0) {
        i <- get_param(mode2, i + 2) + 1
      } else {
        i <- i + 3
      }
    } else if (opcode == 6) {
      if (get_param(mode1, i + 1) == 0) {
        i <- get_param(mode2, i + 2) + 1
      } else {
        i <- i + 3
      }
    } else if (opcode == 7) {
      set_param(mode3, i + 3, ifelse(get_param(mode1, i + 1) < get_param(mode2, i + 2), 1, 0))
      i <- i + 4
    } else if (opcode == 8) {
      set_param(mode3, i + 3, ifelse(get_param(mode1, i + 1) == get_param(mode2, i + 2), 1, 0))
      i <- i + 4
    } else if (opcode == 9) {
      relative_base <- relative_base + get_param(mode1, i + 1)
      i <- i + 2
    } else {
      stop(paste("Invalid opcode:", opcode, "at position:", i))
    }
  }
  return(output)
}

# Read program from input.txt
program <- as.numeric(strsplit(readLines("input.txt"), ",")[[1]])

# Part 1: Run with input 1
output_part1 <- run_intcode(program, 1)
print(paste("Part 1 Output:", output_part1))

# Part 2: Run with input 2
output_part2 <- run_intcode(program, 2)
print(paste("Part 2 Output:", output_part2))
