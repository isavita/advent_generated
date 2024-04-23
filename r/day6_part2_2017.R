banks <- as.integer(strsplit(readLines("input.txt"), "\\s+")[[1]])
seen <- list()
cycles <- 0

while(TRUE) {
  state <- paste(banks, collapse = ",")
  if (isTRUE("%in%"(state, names(seen)))) {
    cat("The size of the loop is", cycles - seen[[state]])
    break
  }
  seen[[state]] <- cycles
  
  maxIndex <- which.max(banks)
  blocks <- banks[maxIndex]
  banks[maxIndex] <- 0
  for (i in 1:blocks) {
    banks[((maxIndex + i - 1) %% length(banks)) + 1] <- banks[((maxIndex + i - 1) %% length(banks)) + 1] + 1
  }
  
  cycles <- cycles + 1
}