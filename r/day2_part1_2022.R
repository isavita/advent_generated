
lines <- readLines("input.txt")

totalScore <- 0

for (line in lines) {
  opponent <- substr(line, 1, 1)
  yourMove <- substr(line, 3, 3)

  score <- 0
  if (yourMove == "X") {
    score <- 1
  } else if (yourMove == "Y") {
    score <- 2
  } else if (yourMove == "Z") {
    score <- 3
  }

  if ((opponent == "A" && yourMove == "Y") || (opponent == "B" && yourMove == "Z") || (opponent == "C" && yourMove == "X")) {
    score <- score + 6
  } else if ((opponent == "A" && yourMove == "X") || (opponent == "B" && yourMove == "Y") || (opponent == "C" && yourMove == "Z")) {
    score <- score + 3
  }

  totalScore <- totalScore + score
}

cat(totalScore)
