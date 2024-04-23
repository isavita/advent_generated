target <- as.integer(readLines("input.txt"))
target <- target / 11

houses <- rep(0, target + 1)

for (elf in 1:target) {
  for (house in seq(elf, min(elf * 50, target), by = elf)) {
    houses[house] <- houses[house] + elf
  }
}

for (house in 1:target) {
  if (houses[house] >= target) {
    print(house)
    break
  }
}