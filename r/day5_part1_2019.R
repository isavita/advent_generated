run_intcode <- function(program, input) {
  program <- as.integer(strsplit(program, ",")[[1]])
  pointer <- 1
  
  while (TRUE) {
    instruction <- program[pointer]
    opcode <- instruction %% 100
    modes <- as.integer(rev(strsplit(as.character(instruction %/% 100), "")[[1]]))
    
    if (opcode == 99) break
    
    get_param <- function(offset) {
      mode <- ifelse(offset <= length(modes), modes[offset], 0)
      value <- program[pointer + offset]
      if (mode == 0) return(program[value + 1])
      return(value)
    }
    
    if (opcode == 3) {
      program[program[pointer + 1] + 1] <- input
      pointer <- pointer + 2
    } else if (opcode == 4) {
      output <- get_param(1)
      print(output)
      pointer <- pointer + 2
    } else {
      param1 <- get_param(1)
      param2 <- get_param(2)
      dest <- program[pointer + 3] + 1
      
      if (opcode == 1) {
        program[dest] <- param1 + param2
        pointer <- pointer + 4
      } else if (opcode == 2) {
        program[dest] <- param1 * param2
        pointer <- pointer + 4
      } else if (opcode == 5) {
        pointer <- ifelse(param1 != 0, param2 + 1, pointer + 3)
      } else if (opcode == 6) {
        pointer <- ifelse(param1 == 0, param2 + 1, pointer + 3)
      } else if (opcode == 7) {
        program[dest] <- ifelse(param1 < param2, 1, 0)
        pointer <- pointer + 4
      } else if (opcode == 8) {
        program[dest] <- ifelse(param1 == param2, 1, 0)
        pointer <- pointer + 4
      }
    }
  }
}

input_data <- readLines("input.txt")
run_intcode(input_data, 1)