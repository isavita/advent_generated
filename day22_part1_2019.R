
input <- readLines("input.txt")

deck <- 0:10006

for (line in input) {
  if (grepl("deal into new stack", line)) {
    deck <- rev(deck)
  } else if (grepl("cut", line)) {
    n <- as.numeric(strsplit(line, " ")[[1]][2])
    if (n > 0) {
      deck <- c(deck[(n+1):length(deck)], deck[1:n])
    } else {
      deck <- c(deck[(length(deck)+n+1):length(deck)], deck[1:(length(deck)+n)])
    }
  } else if (grepl("deal with increment", line)) {
    n <- as.numeric(strsplit(line, " ")[[1]][4])
    new_deck <- integer(length(deck))
    pos <- 0
    for (card in deck) {
      new_deck[pos %% length(deck) + 1] <- card
      pos <- pos + n
    }
    deck <- new_deck
  }
}

print(which(deck == 2019) - 1)
