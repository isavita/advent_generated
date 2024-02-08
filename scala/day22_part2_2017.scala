object Solution {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("input.txt")
    val lines = try source.getLines().toList finally source.close()

    val grid = scala.collection.mutable.Map[(Int, Int), Int]()
    var startX, startY = 0
    for ((line, y) <- lines.zipWithIndex) {
      for ((c, x) <- line.zipWithIndex) {
        if (c == '#') {
          grid((x, y)) = 2
        }
      }
      startX = line.length / 2
      startY = y / 2
    }

    val dx = Array(0, 1, 0, -1)
    val dy = Array(-1, 0, 1, 0)

    var x = startX
    var y = startY
    var dir = 0
    var infectedCount = 0

    for (_ <- 0 until 10000000) {
      val pos = (x, y)
      grid.getOrElse(pos, 0) match {
        case 0 =>
          dir = (dir - 1 + 4) % 4
          grid(pos) = 1
        case 1 =>
          grid(pos) = 2
          infectedCount += 1
        case 2 =>
          dir = (dir + 1) % 4
          grid(pos) = 3
        case 3 =>
          dir = (dir + 2) % 4
          grid(pos) = 0
      }
      x += dx(dir)
      y += dy(dir)
    }

    println(infectedCount)
  }
}