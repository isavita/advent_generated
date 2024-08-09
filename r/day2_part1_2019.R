run_intcode <- function(program) {
  pos <- 1
  while (TRUE) {
    opcode <- program[pos]
    if (opcode == 99) break
    a <- program[program[pos + 1] + 1]
    b <- program[program[pos + 2] + 1]
    output_pos <- program[pos + 3] + 1
    program[output_pos] <- if (opcode == 1) a + b else a * b
    pos <- pos + 4
  }
  return(program[1])
}

main <- function() {
  program <- as.integer(unlist(strsplit(readLines("input.txt"), ",")))
  program[2] <- 12
  program[3] <- 2
  result <- run_intcode(program)
  cat(result, "\n")
}

main()