
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

opcodes <- list(addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)

test_opcode <- function(sample, opcode) {
  registers <- sample[[1]]
  registers <- opcode(registers, sample[[2]][2], sample[[2]][3], sample[[2]][4])
  all(registers == sample[[3]])
}

parse_input <- function(input_file) {
    lines <- readLines(input_file)
    samples <- list()
    i <- 1
    
    while(i < length(lines) && grepl("Before",lines[i])){
        before <- as.integer(strsplit(gsub("[^0-9,]", "", lines[i]), ",")[[1]])
        instruction <- as.integer(strsplit(lines[i+1], " ")[[1]])
        after <- as.integer(strsplit(gsub("[^0-9,]", "", lines[i+2]), ",")[[1]])
        samples <- c(samples, list(list(before, instruction, after)))
        i<-i+4
    }
  
    return(samples)
}

part_one <- function(samples) {
  count <- 0
  for (sample in samples) {
    num_behave_like <- 0
    for (opcode in opcodes) {
      if (test_opcode(sample, opcode)) {
        num_behave_like <- num_behave_like + 1
      }
    }
    if (num_behave_like >= 3) {
      count <- count + 1
    }
  }
  count
}
parse_program <- function(input_file){
    lines <- readLines(input_file)
    program <- list()
    
    for(line in lines){
        if(grepl("^\\d", line)){
            program <- c(program, list(as.integer(strsplit(line," ")[[1]])))
        }
    }
    
    return(program)
}

part_two <- function(samples, program) {
  opcode_mapping <- list()
  possible_opcodes <- vector("list", 16)
  for (i in 1:16) possible_opcodes[[i]] <- 1:16

  for (sample in samples) {
    opcode_num <- sample[[2]][1] + 1
      possible_opcodes[[opcode_num]] <- which(sapply(opcodes, function(op) test_opcode(sample, op)))
  }

  while (length(opcode_mapping) < 16) {
    for (opcode_num in 1:16) {
      if (length(possible_opcodes[[opcode_num]]) == 1) {
        opcode_idx <- possible_opcodes[[opcode_num]][1]
        opcode_mapping[[as.character(opcode_num -1)]] <- opcodes[[opcode_idx]]
        for (j in 1:16) {
            if(length(possible_opcodes[[j]]) > 1) possible_opcodes[[j]] <- setdiff(possible_opcodes[[j]], opcode_idx)

        }
      }
    }
  }

  registers <- c(0, 0, 0, 0)
   for (instruction in program) {
    opcode <- opcode_mapping[[as.character(instruction[1])]]
    registers <- opcode(registers, instruction[2], instruction[3], instruction[4])
  }

  registers[1]
}

main <- function() {
    samples <- parse_input("input.txt")
    program <- parse_program("input.txt")
  
    cat(part_one(samples), "\n")
    cat(part_two(samples, program), "\n")
}

main()
