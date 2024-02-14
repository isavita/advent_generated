
data <- readLines("input.txt")
totalScore <- 0

for (line in data) {
  opponent <- substr(line, 1, 1)
  roundEnd <- substr(line, 3, 3)

  yourMove <- ' '
  if (roundEnd == 'X') {
    if (opponent == 'A') {
      yourMove <- 'Z'
    } else if (opponent == 'B') {
      yourMove <- 'X'
    } else {
      yourMove <- 'Y'
    }
  } else if (roundEnd == 'Y') {
    if (opponent == 'A') {
      yourMove <- 'X'
    } else if (opponent == 'B') {
      yourMove <- 'Y'
    } else {
      yourMove <- 'Z'
    }
  } else {
    if (opponent == 'A') {
      yourMove <- 'Y'
    } else if (opponent == 'B') {
      yourMove <- 'Z'
    } else {
      yourMove <- 'X'
    }
  }

  score <- 0
  if (yourMove == 'X') {
    score <- 1
  } else if (yourMove == 'Y') {
    score <- 2
  } else if (yourMove == 'Z') {
    score <- 3
  }

  if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
    score <- score + 6
  } else if ((opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')) {
    score <- score + 3
  }

  totalScore <- totalScore + score
}

cat(totalScore)
