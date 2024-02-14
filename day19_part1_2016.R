
totalElves <- as.integer(readLines("input.txt"))
highestPowerOfTwo <- 1
while (highestPowerOfTwo * 2 <= totalElves) {
  highestPowerOfTwo <- highestPowerOfTwo * 2
}
winner <- (totalElves - highestPowerOfTwo) * 2 + 1
cat(winner)
