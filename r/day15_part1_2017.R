dueling_generators <- function() {
  input <- readLines("input.txt")
  startA <- as.numeric(sub("Generator A starts with ", "", input[1]))
  startB <- as.numeric(sub("Generator B starts with ", "", input[2]))
  
  a <- startA
  b <- startB
  matches <- 0
  
  for (i in 1:40000000) {
    a <- (a * 16807) %% 2147483647
    b <- (b * 48271) %% 2147483647
    
    if ((a %% 65536) == (b %% 65536)) {
      matches <- matches + 1
    }
  }
  
  cat(matches, "\n")
}

dueling_generators()