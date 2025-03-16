
# Intcode computer implementation (optimized)
intcode_computer <- function(program, input = numeric(0)) {
  memory <- program
  pc <- 1
  relative_base <- 0
  input_ptr <- 1
  output_values <- numeric(0)
  
  get_value <- function(mode, offset) {
    address <- if (mode == 0) memory[pc + offset] + 1 else if (mode == 1) pc + offset else memory[pc + offset] + relative_base + 1
    if(address > length(memory)) {
      memory <<- c(memory, rep(0, address - length(memory)))
    }
    memory[address]
  }
  
  set_value <- function(mode, offset, value) {
    address <- if (mode == 0) memory[pc + offset] + 1 else if (mode == 1) pc + offset else memory[pc + offset] + relative_base + 1
    if(address > length(memory)) {
      memory <<- c(memory, rep(0, address - length(memory)))
    }    
    memory[address] <<- value
  }
  
  while (memory[pc] != 99) {
    opcode <- memory[pc] %% 100
    modes <- as.integer(strsplit(sprintf("%05d", memory[pc]), "")[[1]])[1:3]
    modes <- rev(modes) # Modes are right-to-left
    
    if (opcode == 1) {
      set_value(modes[3], 3, get_value(modes[1], 1) + get_value(modes[2], 2))
      pc <- pc + 4
    } else if (opcode == 2) {
      set_value(modes[3], 3, get_value(modes[1], 1) * get_value(modes[2], 2))
      pc <- pc + 4
    } else if (opcode == 3) {
        if (input_ptr > length(input)) {
            return(list(output = output_values, state = "waiting", memory = memory, pc = pc, relative_base = relative_base)) #Need more input
        }
      set_value(modes[1], 1, input[input_ptr])
      input_ptr <- input_ptr + 1
      pc <- pc + 2
    } else if (opcode == 4) {
      output_values <- c(output_values, get_value(modes[1], 1))
      pc <- pc + 2
    } else if (opcode == 5) {
      if (get_value(modes[1], 1) != 0) {
        pc <- get_value(modes[2], 2) + 1
      } else {
        pc <- pc + 3
      }
    } else if (opcode == 6) {
      if (get_value(modes[1], 1) == 0) {
        pc <- get_value(modes[2], 2) + 1
      } else {
        pc <- pc + 3
      }
    } else if (opcode == 7) {
      set_value(modes[3], 3, ifelse(get_value(modes[1], 1) < get_value(modes[2], 2), 1, 0))
      pc <- pc + 4
    } else if (opcode == 8) {
      set_value(modes[3], 3, ifelse(get_value(modes[1], 1) == get_value(modes[2], 2), 1, 0))
      pc <- pc + 4
    } else if (opcode == 9) {
      relative_base <- relative_base + get_value(modes[1], 1)
      pc <- pc + 2
    } else {
      stop(paste("Invalid opcode:", opcode))
    }
  }
  return(list(output = output_values, state = "halted", memory = memory, pc = pc, relative_base = relative_base))
}



main <- function() {
  # Read the Intcode program from input.txt
  program_str <- readLines("input.txt")
  program <- as.numeric(unlist(strsplit(program_str, ",")))
  
  # Run the Intcode computer
  result <- intcode_computer(program)
  output <- result$output
  
  # Process the output to count block tiles
  block_count <- 0
  if (length(output) >= 3) {
      for (i in seq(1, length(output) - 2, by = 3)) {
          tile_id <- output[i + 2]
          if (tile_id == 2) {
              block_count <- block_count + 1
          }
      }
  }
  
  # Print the result
  cat(block_count, "\n")
}


# Ensure the main function is called when the script is run
if (!interactive()) {
  main()
}
