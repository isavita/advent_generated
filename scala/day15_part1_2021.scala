
import scala.io.Source

case class Position(x: Int, y: Int, risk: Int)

object Solution extends App {
  val grid = Source.fromFile("input.txt").getLines.map(_.map(_.asDigit).toArray).toArray

  val rows = grid.length
  val cols = grid(0).length
  val directions = List(Position(1, 0, 0), Position(0, 1, 0), Position(-1, 0, 0), Position(0, -1, 0))

  val dist = Array.ofDim[Int](rows, cols)
  for (i <- dist.indices; j <- dist(i).indices) {
    dist(i)(j) = Int.MaxValue
  }
  dist(0)(0) = 0

  val pq = scala.collection.mutable.PriorityQueue.empty(Ordering.by((p: Position) => -p.risk))
  pq.enqueue(Position(0, 0, 0))

  def dijkstra(): Int = {
    while (pq.nonEmpty) {
      val curr = pq.dequeue()
      if (curr.x == rows - 1 && curr.y == cols - 1) {
        return curr.risk
      }
      for (d <- directions) {
        val nx = curr.x + d.x
        val ny = curr.y + d.y
        if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
          val nextRisk = curr.risk + grid(nx)(ny)
          if (nextRisk < dist(nx)(ny)) {
            dist(nx)(ny) = nextRisk
            pq.enqueue(Position(nx, ny, nextRisk))
          }
        }
      }
    }
    -1
  }

  println(dijkstra())
}
