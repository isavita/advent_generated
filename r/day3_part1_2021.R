file <- "input.txt"
counts <- rep(list(c(0, 0)), 12)
for (line in readLines(file)) {
  for (i in 1:nchar(line)) {
    if (substr(line, i, i) == "0") {
      counts[[i]][1] <- counts[[i]][1] + 1
    } else {
      counts[[i]][2] <- counts[[i]][2] + 1
    }
  }
}

gammaRate <- 0
epsilonRate <- 0
for (i in 1:12) {
  if (counts[[i]][1] > counts[[i]][2]) {
    gammaRate <- bitwShiftL(1, 12 - i) + gammaRate
  } else {
    epsilonRate <- bitwShiftL(1, 12 - i) + epsilonRate
  }
}

print(gammaRate * epsilonRate)