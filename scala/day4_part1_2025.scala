import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("input.txt").getLines().filter(_.nonEmpty).toArray
  if (lines.isEmpty) {
    println("Empty grid")
  } else {
    val rows = lines.length
    val cols = lines(0).length
    val grid = lines.map(_.toCharArray)
    var count = 0
    for (y <- 0 until rows; x <- 0 until cols if grid(y)(x) == '@') {
      var neighbors = 0
      var dy = -1
      while (dy <= 1) {
        var dx = -1
        while (dx <= 1) {
          if (dx != 0 || dy != 0) {
            val ny = y + dy
            val nx = x + dx
            if (ny >= 0 && ny < rows && nx >= 0 && nx < cols && grid(ny)(nx) == '@')
              neighbors += 1
          }
          dx += 1
        }
        dy += 1
      }
      if (neighbors < 4) count += 1
    }
    println(s"Number of accessible rolls of paper: $count")
  }
}