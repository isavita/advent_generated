
# Advent of Code Day 21: Chronal Conversion
# This R program simulates the device's activation system instructions
# to find the first value that causes it to halt when compared with Register 0.

solve_day21 <- function() {
  # Read the input file
  input_file <- "input.txt"
  if (!file.exists(input_file)) {
    return()
  }
  lines <- readLines(input_file, warn = FALSE)
  if (length(lines) == 0) return()
  
  # Parse the instruction pointer binding
  # Format: #ip 4
  ip_reg <- as.integer(sub("#ip ", "", lines[1]))
  
  # Parse the program instructions
  # Format: op a b c
  program_lines <- lines[-1]
  n_instr <- length(program_lines)
  op_codes <- character(n_instr)
  a_vals <- numeric(n_instr)
  b_vals <- numeric(n_instr)
  c_vals <- numeric(n_instr)
  
  for (i in 1:n_instr) {
    parts <- strsplit(program_lines[i], " ")[[1]]
    op_codes[i] <- parts[1]
    a_vals[i] <- as.numeric(parts[2])
    b_vals[i] <- as.numeric(parts[3])
    c_vals[i] <- as.numeric(parts[4])
  }
  
  # Initialize registers R0 through R5
  regs <- rep(0, 6)
  ip <- 0
  
  # Bitwise helper to convert large unsigned 32nd bit values
  # to R's 32-bit signed integer format used by bitwAnd, bitwOr, etc.
  wrap <- function(x) {
    x <- x %% 4294967296
    if (x >= 2147483648) x - 4294967296 else x
  }
  
  # Simulation Loop
  # For Day 21 Part 1, we stop at the first comparison with Register 0.
  while (ip >= 0 && ip < n_instr) {
    # 1. Update the register bound to the instruction pointer
    regs[ip_reg + 1] <- ip
    
    # 2. Fetch current instruction
    idx <- ip + 1
    op <- op_codes[idx]
    a <- a_vals[idx]
    b <- b_vals[idx]
    c <- c_vals[idx]
    
    # 3. Check for halting condition
    # The program halts if eqrr register X and register 0 results in a match.
    # We find the value in the compared register (X) at that moment.
    if (op == "eqrr") {
      if (a == 0 || b == 0) {
        halt_value <- if (a == 0) regs[b + 1] else regs[a + 1]
        # Using options(scipen=999) and sprintf ensures large integers
        # are not printed in scientific notation.
        cat(sprintf("%.0f\n", halt_value))
        return()
      }
    }
    
    # 4. Execute standard assembly operations
    if (op == "addr") {
      regs[c+1] <- regs[a+1] + regs[b+1]
    } else if (op == "addi") {
      regs[c+1] <- regs[a+1] + b
    } else if (op == "mulr") {
      regs[c+1] <- regs[a+1] * regs[b+1]
    } else if (op == "muli") {
      regs[c+1] <- regs[a+1] * b
    } else if (op == "banr") {
      regs[c+1] <- bitwAnd(wrap(regs[a+1]), wrap(regs[b+1]))
    } else if (op == "bani") {
      regs[c+1] <- bitwAnd(wrap(regs[a+1]), wrap(b))
    } else if (op == "borr") {
      regs[c+1] <- bitwOr(wrap(regs[a+1]), wrap(regs[b+1]))
    } else if (op == "bori") {
      regs[c+1] <- bitwOr(wrap(regs[a+1]), wrap(b))
    } else if (op == "setr") {
      regs[c+1] <- regs[a+1]
    } else if (op == "seti") {
      regs[c+1] <- a
    } else if (op == "gtir") {
      regs[c+1] <- if (a > regs[b+1]) 1 else 0
    } else if (op == "gtri") {
      regs[c+1] <- if (regs[a+1] > b) 1 else 0
    } else if (op == "gtrr") {
      regs[c+1] <- if (regs[a+1] > regs[b+1]) 1 else 0
    } else if (op == "eqir") {
      regs[c+1] <- if (a == regs[b+1]) 1 else 0
    } else if (op == "eqri") {
      regs[c+1] <- if (regs[a+1] == b) 1 else 0
    } else if (op == "eqrr") {
      regs[c+1] <- if (regs[a+1] == regs[b+1]) 1 else 0
    }
    
    # Apply 32-bit unsigned overflow masking (keeps result in [0, 2^32-1])
    regs[c+1] <- regs[c+1] %% 4294967296
    
    # 5. Read IP back from the bound register and increment
    ip <- regs[ip_reg + 1]
    ip <- ip + 1
  }
}

# Run the solver
solve_day21()
