banks <- as.integer(strsplit(readLines("input.txt"), "\\s+")[[1]])
seen <- new.env()
cycles <- 0

while(TRUE) {
  state <- paste(banks, collapse = ",")
  if(exists(state, envir = seen, inherits = FALSE)) break
  assign(state, TRUE, envir = seen)
  
  maxIndex <- which.max(banks) - 1
  blocks <- banks[maxIndex + 1]
  banks[maxIndex + 1] <- 0
  for(i in 1:blocks) {
    banks[((maxIndex + i) %% length(banks)) + 1] <- banks[((maxIndex + i) %% length(banks)) + 1] + 1
  }
  
  cycles <- cycles + 1
}

cat("It takes", cycles, "redistribution cycles to reach a repeated configuration.\n")