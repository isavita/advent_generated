instructions <- readLines("input.txt")
registers <- c(a = 1, b = 0)

i <- 1
while (i <= length(instructions)) {
  parts <- strsplit(instructions[i], " ")[[1]]
  
  switch(parts[1],
         hlf = { registers[parts[2]] <- registers[parts[2]] / 2 },
         tpl = { registers[parts[2]] <- registers[parts[2]] * 3 },
         inc = { registers[parts[2]] <- registers[parts[2]] + 1 },
         jmp = { i <- i + as.integer(parts[2]) - 1 },
         jie = { if (registers[substr(parts[2], 1, 1)] %% 2 == 0) i <- i + as.integer(parts[3]) - 1 },
         jio = { if (registers[substr(parts[2], 1, 1)] == 1) i <- i + as.integer(parts[3]) - 1 }
  )
  
  i <- i + 1
}

cat(registers["b"], "\n")