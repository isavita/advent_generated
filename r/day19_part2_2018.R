
# Define the opcodes
opcodes <- list(
  addr = function(regs, a, b, c) { regs[c + 1] <- regs[a + 1] + regs[b + 1]; regs },
  addi = function(regs, a, b, c) { regs[c + 1] <- regs[a + 1] + b; regs },
  mulr = function(regs, a, b, c) { regs[c + 1] <- regs[a + 1] * regs[b + 1]; regs },
  muli = function(regs, a, b, c) { regs[c + 1] <- regs[a + 1] * b; regs },
  banr = function(regs, a, b, c) { regs[c + 1] <- bitwAnd(regs[a + 1], regs[b + 1]); regs },
  bani = function(regs, a, b, c) { regs[c + 1] <- bitwAnd(regs[a + 1], b); regs },
  borr = function(regs, a, b, c) { regs[c + 1] <- bitwOr(regs[a + 1], regs[b + 1]); regs },
  bori = function(regs, a, b, c) { regs[c + 1] <- bitwOr(regs[a + 1], b); regs },
  setr = function(regs, a, b, c) { regs[c + 1] <- regs[a + 1]; regs },
  seti = function(regs, a, b, c) { regs[c + 1] <- a; regs },
  gtir = function(regs, a, b, c) { regs[c + 1] <- ifelse(a > regs[b + 1], 1, 0); regs },
  gtri = function(regs, a, b, c) { regs[c + 1] <- ifelse(regs[a + 1] > b, 1, 0); regs },
  gtrr = function(regs, a, b, c) { regs[c + 1] <- ifelse(regs[a + 1] > regs[b + 1], 1, 0); regs },
  eqir = function(regs, a, b, c) { regs[c + 1] <- ifelse(a == regs[b + 1], 1, 0); regs },
  eqri = function(regs, a, b, c) { regs[c + 1] <- ifelse(regs[a + 1] == b, 1, 0); regs },
  eqrr = function(regs, a, b, c) { regs[c + 1] <- ifelse(regs[a + 1] == regs[b + 1], 1, 0); regs }
)

# Function to execute the program
execute_program <- function(ip_reg, instructions, initial_reg0 = 0) {
  regs <- rep(0, 6)
  regs[1] <- initial_reg0
  ip <- 0
  
  while (ip >= 0 && ip < length(instructions)) {
    regs[ip_reg + 1] <- ip
    
    instr <- instructions[[ip + 1]]
    op <- opcodes[[instr$op]]
    regs <- op(regs, instr$a, instr$b, instr$c)
    
    ip <- regs[ip_reg + 1]
    ip <- ip + 1
  }
  
  return(regs[1])
}


# Optimized Part 2 function.  The given program calculates the sum of divisors.
optimized_part2 <- function(ip_reg, instructions) {
  
  # Run until ip is 3 (beginning of loop that determines the large number)
    regs <- rep(0, 6)
    regs[1] <- 1
    ip <- 0
    
     while (ip >= 0 && ip < length(instructions)) {
      
      regs[ip_reg + 1] <- ip
      instr <- instructions[[ip + 1]]
      op <- opcodes[[instr$op]]
      regs <- op(regs, instr$a, instr$b, instr$c)
      ip <- regs[ip_reg + 1]
      ip <- ip+1
      if(ip==3) break # after this the large number to find the divisors of will be in reg[5]
     }
    
    target_num <- regs[6]
    
    sum_divisors <- 0
    for (i in 1:target_num) {
      if (target_num %% i == 0) {
          sum_divisors <- sum_divisors + i
      }
    }

    
    return (sum_divisors)
}


main <- function() {
  # Read input from file
  input_lines <- readLines("input.txt")
  
  # Parse input
  ip_reg <- as.integer(gsub("#ip ", "", input_lines[1]))
  instructions <- list()
  for (i in 2:length(input_lines)) {
    parts <- strsplit(input_lines[i], " ")[[1]]
    instructions[[i - 1]] <- list(
      op = parts[1],
      a = as.integer(parts[2]),
      b = as.integer(parts[3]),
      c = as.integer(parts[4])
    )
  }
  
  # Part 1
  part1_result <- execute_program(ip_reg, instructions)
  cat("Part 1:", part1_result, "\n")
  
  # Part 2 - Optimized version
  part2_result <- optimized_part2(ip_reg, instructions)
  cat("Part 2:", part2_result, "\n")
}

main()
