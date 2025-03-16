
# Intcode interpreter
run_intcode <- function(program, input_val) {
  memory <- program
  pc <- 1
  relative_base <- 0
  output <- numeric(0)
  
  get_param <- function(mode, pos) {
    if (mode == 0) {
      return(memory[memory[pos] + 1])
    } else if (mode == 1) {
      return(memory[pos])
    } else if (mode == 2) {
      return(memory[memory[pos] + relative_base + 1])
    }
    stop("Invalid parameter mode")
  }
  
  set_param <- function(mode, pos, value) {
    if (mode == 0) {
      memory[memory[pos] + 1] <<- value
    } else if (mode == 2) {
      memory[memory[pos] + relative_base + 1] <<- value
    } else {
      stop("Invalid parameter mode for set")
    }
  }

    input_counter <- 1
  
  while (memory[pc] != 99) {
    opcode <- memory[pc] %% 100
    mode1 <- (memory[pc] %/% 100) %% 10
    mode2 <- (memory[pc] %/% 1000) %% 10
    mode3 <- (memory[pc] %/% 10000) %% 10
    
    if (opcode == 1) {
      param1 <- get_param(mode1, pc + 1)
      param2 <- get_param(mode2, pc + 2)
      set_param(mode3, pc + 3, param1 + param2)
      pc <- pc + 4
    } else if (opcode == 2) {
      param1 <- get_param(mode1, pc + 1)
      param2 <- get_param(mode2, pc + 2)
      set_param(mode3, pc + 3, param1 * param2)
      pc <- pc + 4
    } else if (opcode == 3) {
      set_param(mode1, pc + 1, input_val[input_counter])
      input_counter <- input_counter + 1
      pc <- pc + 2
    } else if (opcode == 4) {
      param1 <- get_param(mode1, pc + 1)
      output <- c(output, param1)
      pc <- pc + 2
    } else if (opcode == 5) {
      param1 <- get_param(mode1, pc + 1)
      param2 <- get_param(mode2, pc + 2)
      if (param1 != 0) {
        pc <- param2 + 1
      } else {
        pc <- pc + 3
      }
    } else if (opcode == 6) {
      param1 <- get_param(mode1, pc + 1)
      param2 <- get_param(mode2, pc + 2)
      if (param1 == 0) {
        pc <- param2 + 1
      } else {
        pc <- pc + 3
      }
    } else if (opcode == 7) {
      param1 <- get_param(mode1, pc + 1)
      param2 <- get_param(mode2, pc + 2)
      set_param(mode3, pc + 3, ifelse(param1 < param2, 1, 0))
      pc <- pc + 4
    } else if (opcode == 8) {
      param1 <- get_param(mode1, pc + 1)
      param2 <- get_param(mode2, pc + 2)
      set_param(mode3, pc + 3, ifelse(param1 == param2, 1, 0))
      pc <- pc + 4
    } else if (opcode == 9) {
      param1 <- get_param(mode1, pc + 1)
      relative_base <- relative_base + param1
      pc <- pc + 2
    }
    else {
      stop(paste("Unknown opcode:", opcode))
    }
  }
  
  return(output)
}

# Main function
main <- function() {
  # Read the program from input.txt
  program_str <- readLines("input.txt")
  program <- as.numeric(unlist(strsplit(program_str, ",")))
    
  # Ensure memory is large enough.  Pad with zeros.
  program <- c(program, rep(0, 10000 - length(program)))

  # Scan the 50x50 area
  affected_points <- 0
  for (x in 0:49) {
    for (y in 0:49) {
      output <- run_intcode(program, c(x, y))
      affected_points <- affected_points + output
    }
  }

  # Print the result
  cat(affected_points, "\n")
}


# Set working directory to script's directory (important for reading input.txt)
if (!interactive()) {
    script_path <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    script_path <- sub(file_arg, "", script_path[grep(file_arg, script_path)])
    setwd(dirname(script_path))
    main()  # Call the main function
}


