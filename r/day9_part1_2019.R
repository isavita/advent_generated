
# Function to parse instruction and parameter modes
parse_instruction <- function(instruction) {
  instruction <- as.character(instruction)
  opcode <- as.numeric(substr(instruction, nchar(instruction) - 1, nchar(instruction)))
  modes <- rev(as.numeric(strsplit(substr(instruction, 1, nchar(instruction) - 2), "")[[1]]))
  
  # Pad modes with zeros if needed
  while(length(modes) < 3) {
    modes <- c(modes, 0)
  }
  
  return(list(opcode = opcode, modes = modes))
}

# Function to get parameter value based on mode
get_parameter_value <- function(memory, address, mode, relative_base) {
  if (mode == 0) { # Position mode
    return(memory[address + 1])
  } else if (mode == 1) { # Immediate mode
    return(address)
  } else if (mode == 2) { # Relative mode
    return(memory[relative_base + address + 1])
  } else {
    stop("Invalid parameter mode")
  }
}

# Function to set parameter value based on mode
set_parameter_value <- function(memory, address, mode, relative_base, value) {
  if (mode == 0) { # Position mode
    memory[address + 1] <- value
  } else if (mode == 2) { # Relative mode
    memory[relative_base + address + 1] <- value
  } else {
    stop("Invalid parameter mode for writing")
  }
  return(memory)
}

# Intcode computer function
run_intcode <- function(program, input_value) {
  memory <- program
  instruction_pointer <- 1
  relative_base <- 0
  output <- numeric()
  
  while (TRUE) {
    instruction <- memory[instruction_pointer]
    parsed <- parse_instruction(instruction)
    opcode <- parsed$opcode
    modes <- parsed$modes
    
    if (opcode == 99) {
      break
    }
    
    if (opcode == 1) { # Addition
      param1 <- get_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base)
      param2 <- get_parameter_value(memory, memory[instruction_pointer + 2], modes[2], relative_base)
      memory <- set_parameter_value(memory, memory[instruction_pointer + 3], modes[3], relative_base, param1 + param2)
      instruction_pointer <- instruction_pointer + 4
    } else if (opcode == 2) { # Multiplication
      param1 <- get_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base)
      param2 <- get_parameter_value(memory, memory[instruction_pointer + 2], modes[2], relative_base)
      memory <- set_parameter_value(memory, memory[instruction_pointer + 3], modes[3], relative_base, param1 * param2)
      instruction_pointer <- instruction_pointer + 4
    } else if (opcode == 3) { # Input
      memory <- set_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base, input_value)
      instruction_pointer <- instruction_pointer + 2
    } else if (opcode == 4) { # Output
      output <- c(output, get_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base))
      instruction_pointer <- instruction_pointer + 2
    } else if (opcode == 5) { # Jump-if-true
      param1 <- get_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base)
      param2 <- get_parameter_value(memory, memory[instruction_pointer + 2], modes[2], relative_base)
      if (param1 != 0) {
        instruction_pointer <- param2 + 1
      } else {
        instruction_pointer <- instruction_pointer + 3
      }
    } else if (opcode == 6) { # Jump-if-false
      param1 <- get_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base)
      param2 <- get_parameter_value(memory, memory[instruction_pointer + 2], modes[2], relative_base)
      if (param1 == 0) {
        instruction_pointer <- param2 + 1
      } else {
        instruction_pointer <- instruction_pointer + 3
      }
    } else if (opcode == 7) { # Less than
      param1 <- get_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base)
      param2 <- get_parameter_value(memory, memory[instruction_pointer + 2], modes[2], relative_base)
      memory <- set_parameter_value(memory, memory[instruction_pointer + 3], modes[3], relative_base, ifelse(param1 < param2, 1, 0))
      instruction_pointer <- instruction_pointer + 4
    } else if (opcode == 8) { # Equals
      param1 <- get_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base)
      param2 <- get_parameter_value(memory, memory[instruction_pointer + 2], modes[2], relative_base)
      memory <- set_parameter_value(memory, memory[instruction_pointer + 3], modes[3], relative_base, ifelse(param1 == param2, 1, 0))
      instruction_pointer <- instruction_pointer + 4
    } else if (opcode == 9) { # Adjust relative base
      param1 <- get_parameter_value(memory, memory[instruction_pointer + 1], modes[1], relative_base)
      relative_base <- relative_base + param1
      instruction_pointer <- instruction_pointer + 2
    } else {
      stop(paste("Unknown opcode:", opcode))
    }
    
    # Expand memory if needed
    if(instruction_pointer > length(memory)){
      memory <- c(memory, rep(0, instruction_pointer - length(memory)))
    }
  }
  
  return(output)
}

# Read program from file
program <- scan("input.txt", sep = ",", quiet = TRUE)

# Run the program with input 1
output <- run_intcode(program, 1)

# Print the output
cat(output, "\n")
