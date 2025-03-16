
# Intcode computer implementation (from previous days, adapted)
run_intcode <- function(program, input_queue = c()) {
  memory <- program
  ip <- 1  # Instruction pointer
  relative_base <- 0
  output_values <- c()
  
  get_param <- function(mode, offset) {
    address <- memory[ip + offset]
    if (mode == 0) { # Position mode
      if (address + 1 > length(memory)) {
        return(0) # Handle out-of-bounds reads
      }
      return(memory[address + 1])
    } else if (mode == 1) { # Immediate mode
      return(address)
    } else if (mode == 2) { # Relative mode
       if (address + relative_base + 1 > length(memory)) {
          return (0)
       }
      return(memory[address + relative_base + 1])
    }
  }
  
  set_param <- function(mode, offset, value) {
    address <- memory[ip + offset]
    if (mode == 0) {
      if(address + 1 > length(memory)) {
        length(memory) <<- address + 1 #grow the memory
      }
      memory[address + 1] <<- value
    } else if (mode == 2) {
      if(address + relative_base + 1 > length(memory)) {
        length(memory) <<- address + relative_base + 1
      }
      memory[address + relative_base + 1] <<- value
    } else {
      stop("Invalid mode for set_param")  # Immediate mode not allowed for writing
    }
  }
  
  while (memory[ip] != 99) {
    opcode <- memory[ip] %% 100
    mode1 <- (memory[ip] %/% 100) %% 10
    mode2 <- (memory[ip] %/% 1000) %% 10
    mode3 <- (memory[ip] %/% 10000) %% 10
    
    if (opcode == 1) { # Add
      set_param(mode3, 3, get_param(mode1, 1) + get_param(mode2, 2))
      ip <- ip + 4
    } else if (opcode == 2) { # Multiply
      set_param(mode3, 3, get_param(mode1, 1) * get_param(mode2, 2))
      ip <- ip + 4
    } else if (opcode == 3) { # Input
      if (length(input_queue) == 0) {
        return(list(output = output_values, state = "waiting", memory = memory, ip = ip, relative_base = relative_base))
      }
      set_param(mode1, 1, input_queue[1])
      input_queue <- input_queue[-1]
      ip <- ip + 2
    } else if (opcode == 4) { # Output
      output_values <- c(output_values, get_param(mode1, 1))
      ip <- ip + 2
    } else if (opcode == 5) { # Jump-if-true
      if (get_param(mode1, 1) != 0) {
        ip <- get_param(mode2, 2) + 1
      } else {
        ip <- ip + 3
      }
    } else if (opcode == 6) { # Jump-if-false
      if (get_param(mode1, 1) == 0) {
        ip <- get_param(mode2, 2) + 1
      } else {
        ip <- ip + 3
      }
    } else if (opcode == 7) { # Less than
      set_param(mode3, 3, ifelse(get_param(mode1, 1) < get_param(mode2, 2), 1, 0))
      ip <- ip + 4
    } else if (opcode == 8) { # Equals
      set_param(mode3, 3, ifelse(get_param(mode1, 1) == get_param(mode2, 2), 1, 0))
      ip <- ip + 4
    } else if (opcode == 9) { # Adjust relative base
      relative_base <- relative_base + get_param(mode1, 1)
      ip <- ip + 2
    } else {
      stop(paste("Unknown opcode:", opcode))
    }
  }
  
  return(list(output = output_values, state = "halted", memory = memory, ip = ip, relative_base = relative_base))
}


# Function to convert springscript to ASCII and run the Intcode program
run_springscript <- function(program, springscript) {
  ascii_input <- unlist(sapply(paste0(springscript, "\n"), function(x) utf8ToInt(x)))
  
  result <- run_intcode(program, ascii_input)
  
  # Print the ASCII output, handling large hull damage value.
  cat(sapply(result$output, function(x) {
          if (x > 127) {
              return(paste("Hull Damage:", x))
          } else {
            return(intToUtf8(x))
          }
      }), sep = "")
  
  return(result)
}

main <- function() {
  # Read the Intcode program from the input file
  input_file <- "input.txt"
  program <- as.numeric(strsplit(readLines(input_file), ",")[[1]])
  
  
  # Springscript program (Part 1)
  # Jump if there's a hole in the next 3 and ground at D
  # (!A OR !B OR !C) AND D
  springscript_part1 <- c(
    "NOT A J", # J = !A
    "NOT B T", # T = !B
    "OR T J",  # J = !A OR !B
    "NOT C T", # T = !C
    "OR T J",   # J = !A OR !B OR !C
    "AND D J",   # J = (!A OR !B OR !C) AND D
    "WALK"
  )
  
  cat("Part 1 Output:\n")
  run_springscript(program, springscript_part1)

  # Part 2: Add a check for ground at H, to jump long gaps
  # (!A OR !B OR !C) AND D AND (E OR H)
  
  # If  A and D is ground no need to jump.
  
  springscript_part2 <- c(
      "NOT A J", # J = !A
      "NOT B T", # T = !B
      "OR T J",  # J = !A OR !B
      "NOT C T", # T = !C
      "OR T J",   # J = !A OR !B OR !C
      "AND D J",   # J = (!A OR !B OR !C) AND D
      
      # Check if we also need to jump larger gaps, checking H
      "NOT J T",
      "OR E T",
      "OR H T",
      "AND T J",
      
      "RUN"   #Use run command for Part2
  )
  
  cat("\nPart 2 Output:\n")
  run_springscript(program, springscript_part2)
}

main()
