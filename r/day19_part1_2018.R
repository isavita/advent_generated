
# Function to perform operations
perform_op <- function(op, a, b, c, registers) {
  switch(op,
         addr = { registers[c+1] <- registers[a+1] + registers[b+1] },
         addi = { registers[c+1] <- registers[a+1] + b },
         mulr = { registers[c+1] <- registers[a+1] * registers[b+1] },
         muli = { registers[c+1] <- registers[a+1] * b },
         banr = { registers[c+1] <- bitwAnd(registers[a+1], registers[b+1]) },
         bani = { registers[c+1] <- bitwAnd(registers[a+1], b) },
         borr = { registers[c+1] <- bitwOr(registers[a+1], registers[b+1]) },
         bori = { registers[c+1] <- bitwOr(registers[a+1], b) },
         setr = { registers[c+1] <- registers[a+1] },
         seti = { registers[c+1] <- a },
         gtir = { registers[c+1] <- ifelse(a > registers[b+1], 1, 0) },
         gtri = { registers[c+1] <- ifelse(registers[a+1] > b, 1, 0) },
         gtrr = { registers[c+1] <- ifelse(registers[a+1] > registers[b+1], 1, 0) },
         eqir = { registers[c+1] <- ifelse(a == registers[b+1], 1, 0) },
         eqri = { registers[c+1] <- ifelse(registers[a+1] == b, 1, 0) },
         eqrr = { registers[c+1] <- ifelse(registers[a+1] == registers[b+1], 1, 0) }
  )
  return(registers)
}

# Read input from file
input <- readLines("input.txt")

# Parse input
ip_reg <- as.integer(gsub("#ip ", "", input[1]))
instructions <- strsplit(input[-1], " ")
instructions <- lapply(instructions, function(x) {
  list(op = x[1], a = as.integer(x[2]), b = as.integer(x[3]), c = as.integer(x[4]))
})

# Initialize registers and instruction pointer
registers <- rep(0, 6)
ip <- 0

# Execute program
while (ip >= 0 && ip < length(instructions)) {
  
  # Write IP to bound register
  registers[ip_reg + 1] <- ip
  
  # Get current instruction
  instr <- instructions[[ip + 1]]
  
  # Execute instruction
  registers <- perform_op(instr$op, instr$a, instr$b, instr$c, registers)
  
  # Update IP from bound register
  ip <- registers[ip_reg + 1]
  
  # Increment IP
  ip <- ip + 1
}

# Print the value of register 0
cat(registers[1], "\n")
