
input <- readLines("input.txt")

registers <- list()
max_value <- 0

for (line in input) {
  parts <- strsplit(line, " ")[[1]]
  reg <- parts[1]
  action <- parts[2]
  amount <- as.integer(parts[3])
  cond_reg <- parts[5]
  cond_op <- parts[6]
  cond_val <- as.integer(parts[7])
  
  if (!(reg %in% names(registers))) {
    registers[[reg]] <- 0
  }
  if (!(cond_reg %in% names(registers))) {
    registers[[cond_reg]] <- 0
  }
  
  if (cond_op == ">" && registers[[cond_reg]] > cond_val) {
    if (action == "inc") {
      registers[[reg]] <- registers[[reg]] + amount
    } else {
      registers[[reg]] <- registers[[reg]] - amount
    }
  } else if (cond_op == "<" && registers[[cond_reg]] < cond_val) {
    if (action == "inc") {
      registers[[reg]] <- registers[[reg]] + amount
    } else {
      registers[[reg]] <- registers[[reg]] - amount
    }
  } else if (cond_op == ">=" && registers[[cond_reg]] >= cond_val) {
    if (action == "inc") {
      registers[[reg]] <- registers[[reg]] + amount
    } else {
      registers[[reg]] <- registers[[reg]] - amount
    }
  } else if (cond_op == "<=" && registers[[cond_reg]] <= cond_val) {
    if (action == "inc") {
      registers[[reg]] <- registers[[reg]] + amount
    } else {
      registers[[reg]] <- registers[[reg]] - amount
    }
  } else if (cond_op == "==" && registers[[cond_reg]] == cond_val) {
    if (action == "inc") {
      registers[[reg]] <- registers[[reg]] + amount
    } else {
      registers[[reg]] <- registers[[reg]] - amount
    }
  } else if (cond_op == "!=" && registers[[cond_reg]] != cond_val) {
    if (action == "inc") {
      registers[[reg]] <- registers[[reg]] + amount
    } else {
      registers[[reg]] <- registers[[reg]] - amount
    }
  }
  
  max_value <- max(max_value, max(unlist(registers)))
}

print(max_value)
