
import java.io.File
import scala.io.Source
import scala.collection.mutable
import scala.math.min

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    val n = lines.length
    val m = lines(0).length
    var sx = 0
    var sy = 0
    var ex = 0
    var ey = 0

    for (i <- 0 until n; j <- 0 until m) {
      if (lines(i)(j) == 'S') {
        sx = i
        sy = j
      } else if (lines(i)(j) == 'E') {
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
        
      } else if (u.x == ex && u.y == ey) {
        println(u.cost)
        return
      } else {
        for (ndir <- Array((u.d + 1) % 4, (u.d + 3) % 4)) {
          val nc = u.cost + 1000
          if (nc < dist(u.x)(u.y)(ndir)) {
            dist(u.x)(u.y)(ndir) = nc
            pq.enqueue(Node(u.x, u.y, ndir, nc))
          }
        }
        val nx = u.x + dx(u.d)
        val ny = u.y + dy(u.d)
        if (nx >= 0 && nx < n && ny >= 0 && ny < m && lines(nx)(ny) != '#') {
          val nc = u.cost + 1
          if (nc < dist(nx)(ny)(u.d)) {
            dist(nx)(ny)(u.d) = nc
            pq.enqueue(Node(nx, ny, u.d, nc))
          }
        }
      }
    }
  }

  case class Node(x: Int, y: Int, d: Int, cost: Int)
}
