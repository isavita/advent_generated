
import scala.io.Source

object Main extends App {
  val filename = "input.txt"
  var totalScore = 0

  for (line <- Source.fromFile(filename).getLines) {
    val opponent = line(0)
    val roundEnd = line(2)

    val yourMove = roundEnd match {
      case 'X' => if (opponent == 'A') 'Z' else if (opponent == 'B') 'X' else 'Y'
      case 'Y' => if (opponent == 'A') 'X' else if (opponent == 'B') 'Y' else 'Z'
      case _ => if (opponent == 'A') 'Y' else if (opponent == 'B') 'Z' else 'X'
    }

    val score = yourMove match {
      case 'X' => 1
      case 'Y' => 2
      case 'Z' => 3
    }

    if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
      totalScore += score + 6
    } else if ((opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')) {
      totalScore += score + 3
    } else {
      totalScore += score
    }
  }

  println(totalScore)
}
