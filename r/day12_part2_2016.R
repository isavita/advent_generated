
# Function to execute assembunny code
execute_assembunny <- function(instructions, initial_registers = c(a = 0, b = 0, c = 0, d = 0)) {
  registers <- initial_registers
  instruction_pointer <- 1
  
  while (instruction_pointer >= 1 && instruction_pointer <= length(instructions)) {
    instruction <- instructions[instruction_pointer]
    parts <- strsplit(instruction, " ")[[1]]
    
    opcode <- parts[1]
    
    if (opcode == "cpy") {
      source <- parts[2]
      dest <- parts[3]
      
      if (grepl("^[a-d]$", source)) {
        value <- registers[source]
      } else {
        value <- as.integer(source)
      }
      registers[dest] <- value
      instruction_pointer <- instruction_pointer + 1
      
    } else if (opcode == "inc") {
      reg <- parts[2]
      registers[reg] <- registers[reg] + 1
      instruction_pointer <- instruction_pointer + 1
      
    } else if (opcode == "dec") {
      reg <- parts[2]
      registers[reg] <- registers[reg] - 1
      instruction_pointer <- instruction_pointer + 1
      
    } else if (opcode == "jnz") {
      check <- parts[2]
      jump <- parts[3]
      
      if (grepl("^[a-d]$", check)) {
        check_value <- registers[check]
      } else {
        check_value <- as.integer(check)
      }
      
      if (check_value != 0) {
        instruction_pointer <- instruction_pointer + as.integer(jump)
      } else {
        instruction_pointer <- instruction_pointer + 1
      }
    }
  }
  return(registers)
}

# Read instructions from input.txt
instructions <- readLines("input.txt")

# Part 1: Execute with initial registers all 0
registers_part1 <- execute_assembunny(instructions)
print(paste("Part 1: Value in register a =", registers_part1["a"]))

# Part 2: Execute with register c initialized to 1
registers_part2 <- execute_assembunny(instructions, initial_registers = c(a = 0, b = 0, c = 1, d = 0))
print(paste("Part 2: Value in register a =", registers_part2["a"]))
