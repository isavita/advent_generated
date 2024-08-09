instructions <- readLines("input.txt", warn = FALSE)

producesClockSignal <- function(a) {
  registers <- c(a = a, b = 0, c = 0, d = 0)
  lastOutput <- 0
  outputCount <- 0
  i <- 1
  
  while (i <= length(instructions)) {
    parts <- unlist(strsplit(instructions[i], " "))
    if (parts[1] == "cpy") {
      val <- getValue(parts[2], registers)
      registers[parts[3]] <- val
    } else if (parts[1] == "inc") {
      registers[parts[2]] <- registers[parts[2]] + 1
    } else if (parts[1] == "dec") {
      registers[parts[2]] <- registers[parts[2]] - 1
    } else if (parts[1] == "jnz") {
      val <- getValue(parts[2], registers)
      if (val != 0) {
        i <- i + as.integer(parts[3])
        next
      }
    } else if (parts[1] == "out") {
      val <- getValue(parts[2], registers)
      if (val != 0 && val != 1 || (outputCount > 0 && val == lastOutput)) {
        return(FALSE)
      }
      lastOutput <- val
      outputCount <- outputCount + 1
      if (outputCount > 50) return(TRUE)
    }
    i <- i + 1
  }
  FALSE
}

getValue <- function(s, registers) {
  if (grepl("^[0-9]+$", s)) {
    as.integer(s)
  } else {
    registers[s]
  }
}

a <- 1
while (TRUE) {
  if (producesClockSignal(a)) {
    print(a)
    break
  }
  a <- a + 1
}