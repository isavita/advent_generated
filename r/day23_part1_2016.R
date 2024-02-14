
instructions <- readLines("input.txt")
registers <- list(a = 7, b = 0, c = 0, d = 0)

pc <- 1
while (pc <= length(instructions)) {
  fields <- strsplit(instructions[pc], " ")[[1]]
  instruction <- fields[1]
  
  switch(instruction,
         "cpy" = {
           x <- as.integer(ifelse(is.na(as.integer(fields[2])), registers[[fields[2]]], fields[2]))
           if (!is.na(x)) {
             registers[[fields[3]]] <- x
           }
         },
         "inc" = {
           registers[[fields[2]]] <- registers[[fields[2]]] + 1
         },
         "dec" = {
           registers[[fields[2]]] <- registers[[fields[2]]] - 1
         },
         "jnz" = {
           x <- as.integer(ifelse(is.na(as.integer(fields[2])), registers[[fields[2]]], fields[2]))
           if (x != 0) {
             pc <- pc + as.integer(ifelse(is.na(as.integer(fields[3])), registers[[fields[3]]], fields[3])) - 1
           }
         },
         "tgl" = {
           x <- as.integer(ifelse(is.na(as.integer(fields[2])), registers[[fields[2]]], fields[2]))
           tgt <- pc + x
           if (tgt >= 1 && tgt <= length(instructions)) {
             parts <- strsplit(instructions[tgt], " ")[[1]]
             new_instr <- switch(parts[1],
                                 "inc" = "dec",
                                 "dec" = "inc",
                                 "tgl" = "inc",
                                 "jnz" = "cpy",
                                 "cpy" = "jnz")
             instructions[tgt] <- paste(new_instr, paste(parts[-1], collapse = " "))
           }
         }
  )
  
  pc <- pc + 1
}

print(registers$a)
