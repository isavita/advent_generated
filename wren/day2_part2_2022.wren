import "io" for File

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var totalScore = 0

  for (line in lines) {
    var opponent = line[0]
    var roundEnd = line[2]

    var yourMove = " "
    if (roundEnd == "X") {
      if (opponent == "A") {
        yourMove = "Z"
      } else if (opponent == "B") {
        yourMove = "X"
      } else {
        yourMove = "Y"
      }
    } else if (roundEnd == "Y") {
      if (opponent == "A") {
        yourMove = "X"
      } else if (opponent == "B") {
        yourMove = "Y"
      } else {
        yourMove = "Z"
      }
    } else {
      if (opponent == "A") {
        yourMove = "Y"
      } else if (opponent == "B") {
        yourMove = "Z"
      } else {
        yourMove = "X"
      }
    }

    var score = 0
    if (yourMove == "X") {
      score = 1
    } else if (yourMove == "Y") {
      score = 2
    } else if (yourMove == "Z") {
      score = 3
    }

    if ((opponent == "A" && yourMove == "Y") || (opponent == "B" && yourMove == "Z") || (opponent == "C" && yourMove == "X")) {
      score = score + 6
    } else if ((opponent == "A" && yourMove == "X") || (opponent == "B" && yourMove == "Y") || (opponent == "C" && yourMove == "Z")) {
      score = score + 3
    }

    totalScore = totalScore + score
  }

  System.print(totalScore)
}

main.call()