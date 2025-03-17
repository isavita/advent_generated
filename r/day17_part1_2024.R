
# Function to get the value of a combo operand
get_combo_value <- function(operand, registers) {
  if (operand >= 0 && operand <= 3) {
    return(operand)
  } else if (operand == 4) {
    return(registers$A)
  } else if (operand == 5) {
    return(registers$B)
  } else if (operand == 6) {
    return(registers$C)
  } else {
    stop("Invalid combo operand")  # Should not happen according to problem statement
  }
}

# Function to execute a single instruction
execute_instruction <- function(program, ip, registers, output) {
  opcode <- program[ip + 1]
  operand <- program[ip + 2]
  
  if (opcode == 0) { # adv
    registers$A <- floor(registers$A / (2^get_combo_value(operand, registers)))
    ip <- ip + 2
  } else if (opcode == 1) { # bxl
    registers$B <- bitwXor(registers$B, operand)
    ip <- ip + 2
  } else if (opcode == 2) { # bst
    registers$B <- get_combo_value(operand, registers) %% 8
    ip <- ip + 2
  } else if (opcode == 3) { # jnz
    if (registers$A != 0) {
      ip <- operand
    } else {
      ip <- ip + 2
    }
  } else if (opcode == 4) { # bxc
    registers$B <- bitwXor(registers$B, registers$C)
    ip <- ip + 2
  } else if (opcode == 5) { # out
    output <- c(output, get_combo_value(operand, registers) %% 8)
    ip <- ip + 2
  } else if (opcode == 6) { # bdv
    registers$B <- floor(registers$A / (2^get_combo_value(operand, registers)))
    ip <- ip + 2
  } else if (opcode == 7) { # cdv
    registers$C <- floor(registers$A / (2^get_combo_value(operand, registers)))
    ip <- ip + 2
  } else{
      stop("Invalid opcode")
  }
  
  return(list(ip = ip, registers = registers, output = output))
}

# Main function to run the program
run_program <- function(file_path) {
  # Read input from file
  lines <- readLines(file_path)
  
  # Initialize registers
  registers <- list(A = as.integer(sub(".*: (\\d+)", "\\1", lines[1])),
                    B = as.integer(sub(".*: (\\d+)", "\\1", lines[2])),
                    C = as.integer(sub(".*: (\\d+)", "\\1", lines[3])))

  # Parse program
  program_str <- sub("Program: ", "", lines[5])
  program <- as.integer(strsplit(program_str, ",")[[1]])
  
  # Initialize instruction pointer and output
  ip <- 0
  output <- c()
  
  # Execute program until it halts
  while (ip + 1 <= length(program)) {
      result<- execute_instruction(program, ip, registers, output)
      ip <- result$ip
      registers <- result$registers
      output <- result$output
  }
  
  # Print output
  cat(paste(output, collapse = ","))
  cat("\n")
}

# Main entry point
main <- function() {
  run_program("input.txt")
}

# Run main function
main()

