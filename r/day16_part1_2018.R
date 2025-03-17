
addr <- function(registers, A, B, C) { registers[C + 1] <- registers[A + 1] + registers[B + 1]; registers }
addi <- function(registers, A, B, C) { registers[C + 1] <- registers[A + 1] + B; registers }
mulr <- function(registers, A, B, C) { registers[C + 1] <- registers[A + 1] * registers[B + 1]; registers }
muli <- function(registers, A, B, C) { registers[C + 1] <- registers[A + 1] * B; registers }
banr <- function(registers, A, B, C) { registers[C + 1] <- bitwAnd(registers[A + 1], registers[B + 1]); registers }
bani <- function(registers, A, B, C) { registers[C + 1] <- bitwAnd(registers[A + 1], B); registers }
borr <- function(registers, A, B, C) { registers[C + 1] <- bitwOr(registers[A + 1], registers[B + 1]); registers }
bori <- function(registers, A, B, C) { registers[C + 1] <- bitwOr(registers[A + 1], B); registers }
setr <- function(registers, A, B, C) { registers[C + 1] <- registers[A + 1]; registers }
seti <- function(registers, A, B, C) { registers[C + 1] <- A; registers }
gtir <- function(registers, A, B, C) { registers[C + 1] <- ifelse(A > registers[B + 1], 1, 0); registers }
gtri <- function(registers, A, B, C) { registers[C + 1] <- ifelse(registers[A + 1] > B, 1, 0); registers }
gtrr <- function(registers, A, B, C) { registers[C + 1] <- ifelse(registers[A + 1] > registers[B + 1], 1, 0); registers }
eqir <- function(registers, A, B, C) { registers[C + 1] <- ifelse(A == registers[B + 1], 1, 0); registers }
eqri <- function(registers, A, B, C) { registers[C + 1] <- ifelse(registers[A + 1] == B, 1, 0); registers }
eqrr <- function(registers, A, B, C) { registers[C + 1] <- ifelse(registers[A + 1] == registers[B + 1], 1, 0); registers }

operations <- list(addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)

main <- function() {
  lines <- readLines("input.txt")
  samples <- list()
  i <- 1
  while (i <= length(lines)) {
    if (startsWith(lines[i], "Before:")) {
      before <- as.integer(strsplit(substring(lines[i], 10, nchar(lines[i]) - 1), ", ")[[1]])
      instruction <- as.integer(strsplit(lines[i + 1], " ")[[1]])
      after <- as.integer(strsplit(substring(lines[i + 2], 10, nchar(lines[i + 2]) - 1), ", ")[[1]])
      samples <- c(samples, list(list(before, instruction, after)))
      i <- i + 4
    } else {
      i <- i + 1
    }
  }
  
  count <- 0
  for (sample in samples) {
    before <- sample[[1]]
    instruction <- sample[[2]]
    after <- sample[[3]]
    opcode <- instruction[1]
    A <- instruction[2]
    B <- instruction[3]
    C <- instruction[4]
    matches <- 0
    for (operation in operations) {
      registers <- before
      registers <- operation(registers, A, B, C)
      if (all(registers == after)) {
        matches <- matches + 1
      }
    }
    if (matches >= 3) {
      count <- count + 1
    }
  }
  
  cat(count, "\n")
}

main()
