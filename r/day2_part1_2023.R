
data <- readLines("input.txt")
totalSum <- 0

for (line in data) {
  gameId <- as.numeric(sub("Game (\\d+): .*", "\\1", line))
  rounds <- unlist(strsplit(sub("Game \\d+: (.+)", "\\1", line), ";"))
  isValid <- TRUE

  for (round in rounds) {
    cubes <- gregexpr("(\\d+) (red|green|blue)", round, perl=TRUE)
    cubes <- regmatches(round, cubes)

    red <- 0
    green <- 0
    blue <- 0

    for (cube in cubes[[1]]) {
      count <- as.numeric(sub("(\\d+) (red|green|blue)", "\\1", cube))
      color <- sub("(\\d+) (red|green|blue)", "\\2", cube)

      if (color == "red") {
        red <- red + count
      } else if (color == "green") {
        green <- green + count
      } else if (color == "blue") {
        blue <- blue + count
      }

      if (red > 12 || green > 13 || blue > 14) {
        isValid <- FALSE
        break
      }
    }

    if (!isValid) {
      break
    }
  }

  if (isValid) {
    totalSum <- totalSum + gameId
  }
}

cat(totalSum, "\n")
