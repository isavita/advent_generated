readInput <- function() {
  input <- readLines("input.txt")
  player1Deck <- c()
  player2Deck <- c()
  currentDeck <- "player1"
  
  for (line in input) {
    if (line == "") {
      currentDeck <- "player2"
      next
    }
    if (grepl("Player", line)) {
      next
    }
    card <- as.integer(line)
    if (currentDeck == "player1") {
      player1Deck <- c(player1Deck, card)
    } else {
      player2Deck <- c(player2Deck, card)
    }
  }
  
  list(player1Deck, player2Deck)
}

playRecursiveCombat <- function(player1Deck, player2Deck) {
  previousRounds <- list()
  
  while (length(player1Deck) > 0 && length(player2Deck) > 0) {
    roundKey <- paste(player1Deck, collapse = ",")
    roundKey <- paste(c(roundKey, paste(player2Deck, collapse = ",")), collapse = "|")
    if (roundKey %in% previousRounds) {
      return(list(player1Deck, c()))
    }
    previousRounds <- c(previousRounds, roundKey)
    
    card1 <- player1Deck[1]
    card2 <- player2Deck[1]
    player1Deck <- player1Deck[-1]
    player2Deck <- player2Deck[-1]
    
    if (length(player1Deck) >= card1 && length(player2Deck) >= card2) {
      subPlayer1Deck <- player1Deck[1:card1]
      subPlayer2Deck <- player2Deck[1:card2]
      subWinner <- playRecursiveCombat(subPlayer1Deck, subPlayer2Deck)
      if (length(subWinner[[1]]) > 0) {
        player1Deck <- c(player1Deck, card1, card2)
      } else {
        player2Deck <- c(player2Deck, card2, card1)
      }
    } else {
      if (card1 > card2) {
        player1Deck <- c(player1Deck, card1, card2)
      } else {
        player2Deck <- c(player2Deck, card2, card1)
      }
    }
  }
  
  list(player1Deck, player2Deck)
}

calculateScore <- function(deck) {
  score <- 0
  for (i in seq_along(deck)) {
    score <- score + deck[i] * (length(deck) - i + 1)
  }
  score
}

input <- readInput()
player1Deck <- input[[1]]
player2Deck <- input[[2]]

result <- playRecursiveCombat(player1Deck, player2Deck)
if (length(result[[1]]) > 0) {
  winningDeck <- result[[1]]
} else {
  winningDeck <- result[[2]]
}

print(calculateScore(winningDeck))