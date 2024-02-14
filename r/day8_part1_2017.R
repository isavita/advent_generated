
input <- readLines("input.txt")

registers <- list()
max_value <- 0

for (i in 1:length(input)) {
  parts <- strsplit(input[i], " ")[[1]]
  reg <- parts[1]
  op <- parts[2]
  val <- as.numeric(parts[3])
  cond_reg <- parts[5]
  cond_op <- parts[6]
  cond_val <- as.numeric(parts[7])
  
  if (!(reg %in% names(registers))) {
    registers[[reg]] <- 0
  }
  if (!(cond_reg %in% names(registers))) {
    registers[[cond_reg]] <- 0
  }
  
  if (eval(parse(text = paste0("registers[['", cond_reg, "']] ", cond_op, " ", cond_val)))) {
    if (op == "inc") {
      registers[[reg]] <- registers[[reg]] + val
    } else {
      registers[[reg]] <- registers[[reg]] - val
    }
    max_value <- max(max_value, registers[[reg]])
  }
}

print(max(unlist(registers)))
