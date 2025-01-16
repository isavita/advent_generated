
import scala.io.Source
import scala.collection.mutable

object Solution {
  case class State(x: Int, y: Int, d: Int)
  case class Node(x: Int, y: Int, d: Int, cost: Int)

  def main(args: Array[String]): Unit = {
    val grid = Source.fromFile("input.txt").getLines.toArray
    val n = grid.length
    val m = grid(0).length

    var sx = 0
    var sy = 0
    var ex = 0
    var ey = 0
    for (i <- 0 until n; j <- 0 until m) {
      if (grid(i)(j) == 'S') {
        sx = i
        sy = j
      } else if (grid(i)(j) == 'E') {
        ex = i
        ey = j
      }
    }

    val dx = Array(-1, 0, 1, 0)
    val dy = Array(0, 1, 0, -1)

    val dist = Array.fill(n, m, 4)(Int.MaxValue)
    dist(sx)(sy)(1) = 0

    val pq = mutable.PriorityQueue.empty[Node](Ordering.by(-_.cost))
    pq.enqueue(Node(sx, sy, 1, 0))

    while (pq.nonEmpty) {
      val u = pq.dequeue()
      if (dist(u.x)(u.y)(u.d) < u.cost) {
      } else if (!(u.x == ex && u.y == ey)) {
        for (ndir <- Array((u.d + 1) % 4, (u.d + 3) % 4)) {
          val nc = u.cost + 1000
          if (nc < dist(u.x)(u.y)(ndir)) {
            dist(u.x)(u.y)(ndir) = nc
            pq.enqueue(Node(u.x, u.y, ndir, nc))
          }
        }
        val nx = u.x + dx(u.d)
        val ny = u.y + dy(u.d)
        if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid(nx)(ny) != '#') {
          val nc = u.cost + 1
          if (nc < dist(nx)(ny)(u.d)) {
            dist(nx)(ny)(u.d) = nc
            pq.enqueue(Node(nx, ny, u.d, nc))
          }
        }
      }
    }

    val best = (0 until 4).map(dist(ex)(ey)(_)).min

    val used = Array.fill(n, m)(false)
    val rev = mutable.Queue.empty[State]
    for (d <- 0 until 4 if dist(ex)(ey)(d) == best) rev.enqueue(State(ex, ey, d))

    val vis = Array.fill(n, m, 4)(false)
    rev.foreach(s => vis(s.x)(s.y)(s.d) = true)

    while (rev.nonEmpty) {
      val u = rev.dequeue()
      used(u.x)(u.y) = true
      val costU = dist(u.x)(u.y)(u.d)

      for (pd <- Array((u.d + 1) % 4, (u.d + 3) % 4)) {
        if (dist(u.x)(u.y)(pd) == costU - 1000 && !vis(u.x)(u.y)(pd)) {
          vis(u.x)(u.y)(pd) = true
          rev.enqueue(State(u.x, u.y, pd))
        }
      }

      val px = u.x - dx(u.d)
      val py = u.y - dy(u.d)
      if (px >= 0 && px < n && py >= 0 && py < m && grid(px)(py) != '#' &&
        dist(px)(py)(u.d) == costU - 1 && !vis(px)(py)(u.d)) {
        vis(px)(py)(u.d) = true
        rev.enqueue(State(px, py, u.d))
      }
    }

    val cnt = (0 until n).flatMap(i => (0 until m).filter(j => used(i)(j) && grid(i)(j) != '#')).length
    println(cnt)
  }
}
