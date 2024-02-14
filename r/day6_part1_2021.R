
fishes <- rep(0, 9)
input <- scan("input.txt", what = character())
fishStrs <- unlist(strsplit(input, ","))
for (fishStr in fishStrs) {
  fish <- as.integer(fishStr)
  fishes[fish + 1] <- fishes[fish + 1] + 1
}

for (day in 1:80) {
  newFish <- fishes[1]
  for (i in 2:9) {
    fishes[i - 1] <- fishes[i]
  }
  fishes[7] <- fishes[7] + newFish
  fishes[9] <- newFish
}

totalFish <- sum(fishes)
cat(totalFish, "\n")
