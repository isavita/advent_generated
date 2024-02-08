object Solution extends App {
  import scala.io.Source

  case class Position(x: Int, y: Int)

  val source = Source.fromFile("input.txt")
  val lines = source.getLines().toList
  source.close()

  var grid = Map[Position, Boolean]() // true: infected, false: clean
  var startX, startY = 0
  for {
    (line, y) <- lines.zipWithIndex
    (c, x) <- line.zipWithIndex
  } {
    if (c == '#') grid += Position(x, y) -> true
    startX = line.length / 2
    startY = y / 2
  }

  // Directions: up, right, down, left
  val dx = Array(0, 1, 0, -1)
  val dy = Array(-1, 0, 1, 0)

  var x = startX
  var y = startY
  var dir = 0 // Start facing up
  var infectedCount = 0

  for (_ <- 0 until 10000) {
    val pos = Position(x, y)
    if (grid.contains(pos)) { // infected
      dir = (dir + 1) % 4 // Turn right
      grid -= pos // Clean
    } else { // clean
      dir = (dir - 1 + 4) % 4 // Turn left
      grid += pos -> true // Infect
      infectedCount += 1
    }
    x += dx(dir)
    y += dy(dir) // Move forward
  }

  println(infectedCount)
}