
# Intcode interpreter
intcode_run <- function(program, input_values = c()) {
  program_state <- as.numeric(strsplit(program, ",")[[1]])
  
  # Pad with zeros for extra memory
  program_state <- c(program_state, rep(0, 10000 - length(program_state))) 
  
  
  i <- 1
  relative_base <- 0
  output <- c()
  input_index <- 1

  get_param <- function(program_state, mode, param_index, relative_base) {
    address <- i + param_index
    if (mode == 0) {
      # Position mode
      return(program_state[program_state[address] + 1])
    } else if (mode == 1) {
      # Immediate mode
      return(program_state[address])
    } else if (mode == 2) {
      # Relative mode
      return(program_state[program_state[address] + relative_base + 1])
    }
    stop("Invalid parameter mode")
  }

  set_param <- function(program_state, mode, param_index, relative_base, value) {
    address <- i + param_index
      if (mode == 0) {
      program_state[program_state[address] + 1] <<- value
    } else if (mode == 2) {
      program_state[program_state[address] + relative_base + 1] <<- value
    } else {
       stop("Invalid parameter mode for writing")
    }
  }

  while (program_state[i] != 99) {
    opcode <- program_state[i] %% 100
    mode1 <- (program_state[i] %/% 100) %% 10
    mode2 <- (program_state[i] %/% 1000) %% 10
    mode3 <- (program_state[i] %/% 10000) %% 10

    if (opcode == 1) {
      # Addition
      param1 <- get_param(program_state, mode1, 1, relative_base)
      param2 <- get_param(program_state, mode2, 2, relative_base)
      set_param(program_state, mode3, 3, relative_base, param1 + param2)
      i <- i + 4
    } else if (opcode == 2) {
      # Multiplication
      param1 <- get_param(program_state, mode1, 1, relative_base)
      param2 <- get_param(program_state, mode2, 2, relative_base)
      set_param(program_state, mode3, 3, relative_base, param1 * param2)
      i <- i + 4
    } else if (opcode == 3) {
      # Input
      if (input_index > length(input_values)) {
         stop("Not enough input values provided.")
      }
      set_param(program_state, mode1, 1, relative_base, input_values[input_index])
      input_index <- input_index + 1
      i <- i + 2
    } else if (opcode == 4) {
      # Output
      param1 <- get_param(program_state, mode1, 1, relative_base)
      output <- c(output, param1)
      i <- i + 2
    } else if (opcode == 5) {
      # Jump-if-true
      param1 <- get_param(program_state, mode1, 1, relative_base)
      param2 <- get_param(program_state, mode2, 2, relative_base)
      if (param1 != 0) {
        i <- param2 + 1
      } else {
        i <- i + 3
      }
    } else if (opcode == 6) {
      # Jump-if-false
      param1 <- get_param(program_state, mode1, 1, relative_base)
      param2 <- get_param(program_state, mode2, 2, relative_base)
      if (param1 == 0) {
        i <- param2 + 1
      } else {
        i <- i + 3
      }
    } else if (opcode == 7) {
      # Less than
      param1 <- get_param(program_state, mode1, 1, relative_base)
      param2 <- get_param(program_state, mode2, 2, relative_base)
      if (param1 < param2) {
        set_param(program_state, mode3, 3, relative_base, 1)
      } else {
        set_param(program_state, mode3, 3, relative_base, 0)
      }
      i <- i + 4
    } else if (opcode == 8) {
      # Equals
      param1 <- get_param(program_state, mode1, 1, relative_base)
      param2 <- get_param(program_state, mode2, 2, relative_base)
      if (param1 == param2) {
        set_param(program_state, mode3, 3, relative_base, 1)
      } else {
        set_param(program_state, mode3, 3, relative_base, 0)
      }
      i <- i + 4
    } else if (opcode == 9) {
      # Adjust relative base
      param1 <- get_param(program_state, mode1, 1, relative_base)
      relative_base <- relative_base + param1
      i <- i + 2
    } else {
      stop(paste("Unknown opcode:", opcode))
    }
  }

  return(output)
}


# Function to convert springscript to ASCII codes
springscript_to_ascii <- function(springscript) {
  ascii_codes <- c()
  for (line in springscript) {
    for (char in strsplit(line, "")[[1]]) {
      ascii_codes <- c(ascii_codes, as.integer(charToRaw(char)))
    }
    ascii_codes <- c(ascii_codes, 10)  # Newline after each line
  }
  return(ascii_codes)
}


main <- function() {
    # Read the Intcode program from the file
    program <- readLines("input.txt")

    # Part 1: Springscript for WALK mode
    springscript_walk <- c(
      "NOT A J",
      "NOT B T",
      "OR T J",
      "NOT C T",
      "OR T J",
      "AND D J",
      "WALK"
    )
    
    ascii_input_walk <- springscript_to_ascii(springscript_walk)
    output_walk <- intcode_run(program, ascii_input_walk)
     # Print ASCII output or the final damage value if its large
    if (max(output_walk) > 255) {
          cat("Part 1 Hull damage:", tail(output_walk, 1), "\n")
      } else {
          cat("Part 1 ASCII output:\n")
        cat(rawToChar(as.raw(output_walk)), "\n")
    }


     # Part 2: Springscript for RUN mode
    springscript_run <- c(
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT E T",
        "NOT T T",
        "OR H T",
        "AND T J",
        "RUN"
    )
    
    ascii_input_run <- springscript_to_ascii(springscript_run)
    output_run <- intcode_run(program, ascii_input_run)
    
      # Print ASCII output or the final damage value if its large
    if (max(output_run) > 255) {
          cat("Part 2 Hull damage:", tail(output_run, 1), "\n")
      } else {
          cat("Part 2 ASCII output:\n")
          cat(rawToChar(as.raw(output_run)), "\n")
    }
}

# Run the main function
main()
