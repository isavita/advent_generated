
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  val grid = Array.ofDim[Boolean](1000, 1000)

  def turnOn(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    for {
      x <- x1 to x2
      y <- y1 to y2
    } grid(x)(y) = true
  }

  def turnOff(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    for {
      x <- x1 to x2
      y <- y1 to y2
    } grid(x)(y) = false
  }

  def toggle(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
    for {
      x <- x1 to x2
      y <- y1 to y2
    } grid(x)(y) = !grid(x)(y)
  }

  for (line <- input) {
    val pattern = "([a-z ]+) (\\d+),(\\d+) through (\\d+),(\\d+)".r
    line match {
      case pattern("turn on", x1, y1, x2, y2) => turnOn(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      case pattern("turn off", x1, y1, x2, y2) => turnOff(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      case pattern("toggle", x1, y1, x2, y2) => toggle(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
  }

  val result = grid.flatten.count(_ == true)
  println(result)
}
