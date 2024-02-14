
data <- readLines("input.txt")
input <- as.numeric(unlist(strsplit(data, "")))

total_cups <- 1000000
total_moves <- 10000000

cups <- c(rep(0, total_cups+1))
last_cup <- 0

for (i in 1:length(input)) {
  cup <- as.numeric(input[i])
  if (i > 1) {
    cups[last_cup] <- cup
  }
  last_cup <- cup
}

for (i in (length(input)+1):total_cups) {
  cups[last_cup] <- i
  last_cup <- i
}
cups[last_cup] <- as.numeric(input[1])

current_cup <- as.numeric(input[1])
for (i in 1:total_moves) {
  pickup1 <- cups[current_cup]
  pickup2 <- cups[pickup1]
  pickup3 <- cups[pickup2]

  cups[current_cup] <- cups[pickup3]

  destination_cup <- current_cup - 1
  if (destination_cup == 0) {
    destination_cup <- total_cups
  }
  while (destination_cup == pickup1 || destination_cup == pickup2 || destination_cup == pickup3) {
    destination_cup <- destination_cup - 1
    if (destination_cup == 0) {
      destination_cup <- total_cups
    }
  }

  cups[pickup3] <- cups[destination_cup]
  cups[destination_cup] <- pickup1

  current_cup <- cups[current_cup]
}

cup1 <- cups[1]
cup2 <- cups[cup1]
cat(cup1 * cup2, "\n")
