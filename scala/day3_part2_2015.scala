import scala.io.Source

object Main extends App {
  val directions = Source.fromFile("input.txt").getLines().mkString
  var visitedHouses = scala.collection.mutable.Map[(Int, Int), Boolean]()
  var xSanta = 0
  var ySanta = 0
  var xRobo = 0
  var yRobo = 0
  var isSantaTurn = true

  visitedHouses += ((xSanta, ySanta) -> true)

  for (dir <- directions) {
    var x = 0
    var y = 0
    if (isSantaTurn) {
      x = xSanta
      y = ySanta
    } else {
      x = xRobo
      y = yRobo
    }

    dir match {
      case '^' => y += 1
      case 'v' => y -= 1
      case '>' => x += 1
      case '<' => x -= 1
    }

    visitedHouses += ((x, y) -> true)
    if (isSantaTurn) {
      xSanta = x
      ySanta = y
    } else {
      xRobo = x
      yRobo = y
    }

    isSantaTurn = !isSantaTurn
  }

  println(visitedHouses.size)
}