input <- readLines("input.txt")
player1Start <- as.integer(sub(".*: (\\d+).*", "\\1", input[1]))
player2Start <- as.integer(sub(".*: (\\d+).*", "\\1", input[2]))

player1Pos <- player1Start
player2Pos <- player2Start

player1Score <- 0
player2Score <- 0

dieRoll <- 1
rollCount <- 0

while(TRUE) {
  # Player 1
  rolls <- sum((dieRoll:(dieRoll+2)) %% 100)
  rollCount <- rollCount + 3
  dieRoll <- (dieRoll + 3) %% 100
  
  player1Pos <- ((player1Pos + rolls - 1) %% 10) + 1
  player1Score <- player1Score + player1Pos
  
  if (player1Score >= 1000) {
    cat("Result:", player2Score * rollCount, "\n")
    break
  }
  
  # Player 2
  rolls <- sum((dieRoll:(dieRoll+2)) %% 100)
  rollCount <- rollCount + 3
  dieRoll <- (dieRoll + 3) %% 100
  
  player2Pos <- ((player2Pos + rolls - 1) %% 10) + 1
  player2Score <- player2Score + player2Pos
  
  if (player2Score >= 1000) {
    cat(player1Score * rollCount, "\n")
    break
  }
}