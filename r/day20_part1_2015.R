target <- as.integer(readLines("input.txt")) / 10
houses <- rep(0, target + 1)

for (elf in 1:target) {
  for (house in seq(from = elf, to = target, by = elf)) {
    houses[house] <- houses[house] + elf
  }
}

for (house in 1:length(houses)) {
  if (houses[house] >= target) {
    print(house)
    break
  }
}