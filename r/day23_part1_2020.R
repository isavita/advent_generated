
input <- readLines("input.txt")
input <- strsplit(input, "")[[1]]
cups <- integer(length(input) + 1)

currentCup <- 0
for (i in 1:length(input)) {
  cup <- as.integer(input[i])
  if (i == 1) {
    currentCup <- cup
  }
  if (i < length(input)) {
    nextCup <- as.integer(input[i + 1])
    cups[cup] <- nextCup
  }
}
firstCup <- as.integer(input[1])
lastCup <- as.integer(input[length(input)])
cups[lastCup] <- firstCup

for (i in 1:100) {
  pickup1 <- cups[currentCup]
  pickup2 <- cups[pickup1]
  pickup3 <- cups[pickup2]

  cups[currentCup] <- cups[pickup3]

  destinationCup <- currentCup - 1
  if (destinationCup < 1) {
    destinationCup <- length(input)
  }
  while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
    destinationCup <- destinationCup - 1
    if (destinationCup < 1) {
      destinationCup <- length(input)
    }
  }

  cups[pickup3] <- cups[destinationCup]
  cups[destinationCup] <- pickup1

  currentCup <- cups[currentCup]
}

cup <- cups[1]
while (cup != 1) {
  cat(cup)
  cup <- cups[cup]
  if (cup == 1) {
    break
  }
}
cat("\n")
