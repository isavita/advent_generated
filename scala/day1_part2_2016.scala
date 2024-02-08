import scala.io.Source

object Main extends App {
  val input = Source.fromFile("input.txt").getLines.mkString
  val instructions = input.split(", ")

  var x = 0
  var y = 0
  var direction = 0
  var visited = Set[(Int, Int)]((0, 0))
  var firstLocationVisitedTwice: Option[(Int, Int)] = None

  for (instruction <- instructions) {
    val turn = if (instruction.head == 'R') 1 else -1
    direction = (direction + turn + 4) % 4
    val distance = instruction.tail.toInt

    for (_ <- 1 to distance) {
      direction match {
        case 0 => y += 1
        case 1 => x += 1
        case 2 => y -= 1
        case 3 => x -= 1
      }

      val currentLocation = (x, y)
      if (firstLocationVisitedTwice.isEmpty && visited.contains(currentLocation)) {
        firstLocationVisitedTwice = Some(currentLocation)
      }
      visited += currentLocation
    }
  }

  val distanceToHQ = Math.abs(x) + Math.abs(y)
  val distanceToFirstLocationVisitedTwice = firstLocationVisitedTwice.map { case (x, y) => Math.abs(x) + Math.abs(y) }

  println(distanceToHQ)
  println(distanceToFirstLocationVisitedTwice.getOrElse(0))
}