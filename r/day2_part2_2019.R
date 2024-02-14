
program <- scan("input.txt", what = integer(), sep = ",", quiet = TRUE)

intcode_computer <- function(program, noun, verb) {
  program[2] <- noun
  program[3] <- verb
  
  i <- 1
  while (program[i] != 99) {
    if (program[i] == 1) {
      program[program[i + 3] + 1] <- program[program[i + 1] + 1] + program[program[i + 2] + 1]
    } else if (program[i] == 2) {
      program[program[i + 3] + 1] <- program[program[i + 1] + 1] * program[program[i + 2] + 1]
    }
    i <- i + 4
  }
  
  return(program[1])
}

# Part One
result_part_one <- intcode_computer(program, 12, 2)
print(result_part_one)

# Part Two
target_output <- 19690720
for (noun in 0:99) {
  for (verb in 0:99) {
    if (intcode_computer(program, noun, verb) == target_output) {
      result_part_two <- 100 * noun + verb
      print(result_part_two)
      break
    }
  }
}
