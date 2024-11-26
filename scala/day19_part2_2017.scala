
import scala.io.Source

object Solution extends App {
  val grid = Source.fromFile("input.txt").getLines().map(_.toArray).toArray

  var x = grid(0).indexWhere(_ == '|')
  var y = 0
  var dx = 0
  var dy = 1
  var steps = 0

  while (x >= 0 && x < grid(0).length && y >= 0 && y < grid.length) {
    val cell = grid(y)(x)

    if (cell == ' ') {
      println(steps)
      System.exit(0)
    }

    steps += 1

    if (cell == '+') {
      if (dx == 0) {
        dx = if (x > 0 && (grid(y)(x - 1) == '-' || grid(y)(x - 1).isUpper)) -1 else 1
        dy = 0
      } else {
        dy = if (y > 0 && (grid(y - 1)(x) == '|' || grid(y - 1)(x).isUpper)) -1 else 1
        dx = 0
      }
    }
    x += dx
    y += dy
  }
}
