file <- "input.txt"
player1Deck <- c()
player2Deck <- c()

lines <- readLines(file)
currentDeck <- "player1Deck"

for (line in lines) {
  if (line == "") {
    currentDeck <- "player2Deck"
    next
  }
  if (grepl("Player", line)) {
    next
  }
  card <- as.integer(line)
  if (currentDeck == "player1Deck") {
    player1Deck <- c(player1Deck, card)
  } else {
    player2Deck <- c(player2Deck, card)
  }
}

while (length(player1Deck) > 0 && length(player2Deck) > 0) {
  card1 <- player1Deck[1]
  card2 <- player2Deck[1]
  player1Deck <- player1Deck[-1]
  player2Deck <- player2Deck[-1]
  if (card1 > card2) {
    player1Deck <- c(player1Deck, card1, card2)
  } else {
    player2Deck <- c(player2Deck, card2, card1)
  }
}

winningDeck <- if (length(player1Deck) > 0) player1Deck else player2Deck
score <- sum(winningDeck * (length(winningDeck):1))
print(score)