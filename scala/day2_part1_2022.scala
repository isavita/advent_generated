
import scala.io.Source

object Main extends App {
  val filename = "input.txt"
  val file = Source.fromFile(filename)
  
  var totalScore = 0

  for (line <- file.getLines) {
    val opponent = line(0)
    val yourMove = line(2)

    var score = 0
    if (yourMove == 'X') {
      score = 1
    } else if (yourMove == 'Y') {
      score = 2
    } else if (yourMove == 'Z') {
      score = 3
    }

    if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
      score += 6
    } else if ((opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')) {
      score += 3
    }

    totalScore += score
  }

  file.close()
  println(totalScore)
}
