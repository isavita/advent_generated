object Day13 extends App {
  val favoriteNumber = scala.io.Source.fromFile("input.txt").getLines().next().toInt

  def isWall(x: Int, y: Int): Boolean = {
    val num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
    num.toBinaryString.count(_ == '1') % 2 != 0
  }

  def shortestPathLength(destX: Int, destY: Int): Int = {
    val directions = List((0, 1), (0, -1), (1, 0), (-1, 0))
    var steps = 0
    var visited = Set((1, 1))
    var current = List((1, 1))

    while (!current.contains((destX, destY))) {
      steps += 1
      val next = current.flatMap { case (x, y) =>
        directions.map { case (dx, dy) =>
          (x + dx, y + dy)
        }
      }.filter { case (x, y) =>
        x >= 0 && y >= 0 && !isWall(x, y) && !visited.contains((x, y))
      }

      visited = visited ++ next
      current = next
    }

    steps
  }

  println(shortestPathLength(31, 39))
}