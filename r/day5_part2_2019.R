
# Function to read input from file
read_input <- function(filename) {
  con <- file(filename, "r")
  on.exit(close(con))
  line <- readLines(con, n = 1)
  as.numeric(strsplit(line, ",")[[1]])
}

# Function to get parameter value based on mode
get_param_value <- function(memory, param, mode, relative_base = 0) {
  if (mode == 0) {
    return(memory[param + 1])
  } else if (mode == 1) {
    return(param)
  } else if (mode == 2) {
    return(memory[relative_base + param + 1])
  } else {
    stop("Invalid parameter mode")
  }
}

# Function to set parameter value based on mode
set_param_value <- function(memory, param, mode, value, relative_base = 0) {
  if (mode == 0) {
    memory[param + 1] <- value
  } else if (mode == 2) {
    memory[relative_base + param + 1] <- value
  } else {
    stop("Invalid parameter mode for write")
  }
  return(memory)
}

# Function to run the Intcode program
run_intcode <- function(memory, input_val) {
  i <- 1
  output_values <- c()
  relative_base <- 0
  
  while (i <= length(memory)) {
    instruction <- memory[i]
    opcode <- instruction %% 100
    mode1 <- (instruction %/% 100) %% 10
    mode2 <- (instruction %/% 1000) %% 10
    mode3 <- (instruction %/% 10000) %% 10
    
    if (opcode == 99) {
      break
    } else if (opcode == 1) { # Addition
      param1 <- get_param_value(memory, memory[i + 1], mode1, relative_base)
      param2 <- get_param_value(memory, memory[i + 2], mode2, relative_base)
      memory <- set_param_value(memory, memory[i + 3], mode3, param1 + param2, relative_base)
      i <- i + 4
    } else if (opcode == 2) { # Multiplication
      param1 <- get_param_value(memory, memory[i + 1], mode1, relative_base)
      param2 <- get_param_value(memory, memory[i + 2], mode2, relative_base)
      memory <- set_param_value(memory, memory[i + 3], mode3, param1 * param2, relative_base)
      i <- i + 4
    } else if (opcode == 3) { # Input
      memory <- set_param_value(memory, memory[i + 1], mode1, input_val, relative_base)
      i <- i + 2
    } else if (opcode == 4) { # Output
      output_values <- c(output_values, get_param_value(memory, memory[i + 1], mode1, relative_base))
      i <- i + 2
    } else if (opcode == 5) { # Jump-if-true
      param1 <- get_param_value(memory, memory[i + 1], mode1, relative_base)
      if (param1 != 0) {
        i <- get_param_value(memory, memory[i + 2], mode2, relative_base) + 1
      } else {
        i <- i + 3
      }
    } else if (opcode == 6) { # Jump-if-false
      param1 <- get_param_value(memory, memory[i + 1], mode1, relative_base)
      if (param1 == 0) {
        i <- get_param_value(memory, memory[i + 2], mode2, relative_base) + 1
      } else {
        i <- i + 3
      }
    } else if (opcode == 7) { # Less than
      param1 <- get_param_value(memory, memory[i + 1], mode1, relative_base)
      param2 <- get_param_value(memory, memory[i + 2], mode2, relative_base)
      memory <- set_param_value(memory, memory[i + 3], mode3, ifelse(param1 < param2, 1, 0), relative_base)
      i <- i + 4
    } else if (opcode == 8) { # Equals
      param1 <- get_param_value(memory, memory[i + 1], mode1, relative_base)
      param2 <- get_param_value(memory, memory[i + 2], mode2, relative_base)
      memory <- set_param_value(memory, memory[i + 3], mode3, ifelse(param1 == param2, 1, 0), relative_base)
      i <- i + 4
    } else if (opcode == 9) { # Adjust relative base
      param1 <- get_param_value(memory, memory[i + 1], mode1, relative_base)
      relative_base <- relative_base + param1
      i <- i + 2
    } else {
      stop(paste("Unknown opcode:", opcode, "at position:", i))
    }
  }
  return(output_values)
}

# Main program
memory <- read_input("input.txt")
memory <- c(memory, rep(0, 10000)) # Extend memory for relative mode
output_part1 <- run_intcode(memory, 1)
output_part2 <- run_intcode(memory, 5)

cat("Part 1 Output:", tail(output_part1, n=1), "\n")
cat("Part 2 Output:", tail(output_part2, n=1), "\n")
