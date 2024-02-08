object Day13 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next().toInt

  def isWall(x: Int, y: Int): Boolean = {
    val num = x*x + 3*x + 2*x*y + y + y*y + input
    val count = num.toBinaryString.count(_ == '1')
    count % 2 != 0
  }

  def bfs(start: (Int, Int), target: (Int, Int), maxSteps: Int): Int = {
    val visited = scala.collection.mutable.Set[(Int, Int)]()
    var queue = scala.collection.mutable.Queue[(Int, Int, Int)]()
    queue.enqueue((start._1, start._2, 0))

    while (queue.nonEmpty) {
      val (x, y, steps) = queue.dequeue()
      if (steps > maxSteps) return visited.size

      if (x == target._1 && y == target._2) return steps

      visited.add((x, y))

      val nextSteps = List((x+1, y), (x-1, y), (x, y+1), (x, y-1))
      nextSteps.filter { case (nx, ny) => nx >= 0 && ny >= 0 && !isWall(nx, ny) && !visited((nx, ny)) }
        .foreach { case (nx, ny) => queue.enqueue((nx, ny, steps + 1)) }
    }

    -1
  }

  val part1 = bfs((1, 1), (31, 39), Int.MaxValue)
  val part2 = bfs((1, 1), (Int.MaxValue, Int.MaxValue), 50)

  println(part1)
  println(part2)
}