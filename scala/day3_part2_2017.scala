object Solution extends App {
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.mkString finally source.close()
  val target = lines.trim.toInt

  var grid = Map((0, 0) -> 1)

  var x = 0
  var y = 0
  var dx = 0
  var dy = -1

  while (true) {
    if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
      val tmp = dx
      dx = -dy
      dy = tmp
    }

    x += dx
    y += dy

    var value = 0
    for {
      dx <- -1 to 1
      dy <- -1 to 1
    } {
      value += grid.getOrElse((x + dx, y + dy), 0)
    }

    grid += (x, y) -> value

    if (value > target) {
      println(value)
      sys.exit()
    }
  }
}