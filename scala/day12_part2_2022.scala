import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val grid = scala.collection.mutable.Map[(Int, Int), Char]()
    var start: (Int, Int) = (0, 0)
    var end: (Int, Int) = (0, 0)
    var as: List[(Int, Int)] = List.empty
    var y = 0

    Source.fromFile("input.txt").getLines().foreach { line =>
      line.zipWithIndex.foreach { case (b, x) =>
        val p = (x, y)
        grid(p) = b
        if (b == 'S') {
          start = p
        } else if (b == 'E') {
          end = p
        } else if (b == 'a') {
          as = as :+ p
        }
      }
      y += 1
    }

    grid(start) = 'a'
    grid(end) = 'z'

    val dists = djikstra(grid.toMap, end)

    var l = dists(start)

    for (a <- as) {
      if (dists.contains(a)) {
        l = min(l, dists(a))
      }
    }

    println(l)
  }

  def djikstra(grid: Map[(Int, Int), Char], end: (Int, Int)): Map[(Int, Int), Int] = {
    val pq = scala.collection.mutable.PriorityQueue.empty[(Int, (Int, Int))](
      Ordering.by[(Int, (Int, Int)), Int](-_._1)
    )
    pq.enqueue((0, end))
    var dist = Map(end -> 0)

    while (pq.nonEmpty) {
      val (currDist, curr) = pq.dequeue()
      for (n <- List((0, 1), (0, -1), (1, 0), (-1, 0))) {
        val next = (curr._1 + n._1, curr._2 + n._2)
        if (!grid.contains(next)) {
          // Skip if neighbor is not in grid
        } else if (grid(curr) - grid(next) > 1) {
          // Skip if difference in grid values is too large
        } else {
          val nextDist = dist(curr) + 1
          if (!dist.contains(next) || nextDist < dist(next)) {
            dist += (next -> nextDist)
            pq.enqueue((nextDist, next))
          }
        }
      }
    }

    dist
  }

  def min(a: Int, b: Int): Int = {
    if (a < b) a else b
  }
}