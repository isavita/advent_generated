import scala.io.Source

object Main extends App {
  val directions = Source.fromFile("input.txt").mkString
  var visitedHouses = Set((0, 0))
  var x, y = 0

  for (dir <- directions) {
    dir match {
      case '^' => y += 1
      case 'v' => y -= 1
      case '>' => x += 1
      case '<' => x -= 1
    }
    visitedHouses += ((x, y))
  }

  println(visitedHouses.size)
}