
object Garden {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile("input.txt").getLines().toList
    val (garden, start) = parseData(lines)
    val maxSize = lines.length
    println(calculateNumEnds(garden, start, 26501365, maxSize))
  }

  def parseData(data: List[String]): (Set[(Int, Int)], (Int, Int)) = {
    var garden = Set[(Int, Int)]()
    var start = (0, 0)
    for ((line, y) <- data.zipWithIndex) {
      for ((c, x) <- line.zipWithIndex) {
        if (c != '#') {
          garden += ((x, y))
        }
        if (c == 'S') {
          start = (x, y)
        }
      }
    }
    (garden, start)
  }

  def complexMod(pos: (Int, Int), mod: Int): (Int, Int) = {
    ((pos._1 % mod + mod) % mod, (pos._2 % mod + mod) % mod)
  }

  def calculateNumEnds(garden: Set[(Int, Int)], start: (Int, Int), numIterations: Int, maxSize: Int): Long = {
    var queue = Set(start)
    val done = collection.mutable.ListBuffer[Int]()

    for (i <- 0 until 3 * maxSize) {
      if (i % maxSize == (maxSize - 1) / 2) {
        done += queue.size
      }
      if (done.length == 3) {
        val a = done(0)
        val b = done(1)
        val c = done(2)
        val n = numIterations / maxSize
        return a + n.toLong * (b - a + ((n - 1).toLong * (c - 2 * b + a) / 2))
      }
      var newQueue = Set[(Int, Int)]()
      for (point <- queue) {
        for (dir <- List((1, 0), (-1, 0), (0, 1), (0, -1))) {
          val nextPoint = (point._1 + dir._1, point._2 + dir._2)
          if (garden.contains(complexMod(nextPoint, maxSize))) {
            newQueue += nextPoint
          }
        }
      }
      queue = newQueue
    }

    0 
  }
}
