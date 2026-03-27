
options(scipen = 999)
input <- readLines("input.txt")
p1_start <- as.numeric(sub(".*: ", "", input[1]))
p2_start <- as.numeric(sub(".*: ", "", input[2]))

memo <- new.env(hash = TRUE)
rolls <- table(rowSums(expand.grid(1:3, 1:3, 1:3)))

play <- function(p1, p2, s1, s2, turn) {
  state <- paste(p1, p2, s1, s2, turn, sep = ",")
  if (exists(state, envir = memo)) return(get(state, envir = memo))
  
  if (s1 >= 21) return(c(1, 0))
  if (s2 >= 21) return(c(0, 1))
  
  wins <- c(0, 0)
  for (i in seq_along(rolls)) {
    move <- as.numeric(names(rolls)[i])
    count <- as.numeric(rolls[i])
    
    if (turn == 1) {
      np1 <- (p1 + move - 1) %% 10 + 1
      res <- play(np1, p2, s1 + np1, s2, 2)
    } else {
      np2 <- (p2 + move - 1) %% 10 + 1
      res <- play(p1, np2, s1, s2 + np2, 1)
    }
    wins <- wins + res * count
  }
  
  assign(state, wins, envir = memo)
  return(wins)
}

result <- play(p1_start, p2_start, 0, 0, 1)
cat(sprintf("%.0f\n", max(result)))
