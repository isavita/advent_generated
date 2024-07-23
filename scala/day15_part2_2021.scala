
import scala.collection.mutable
import scala.io.Source

case class Position(x: Int, y: Int, risk: Int)

object Main {
  def main(args: Array[String]): Unit = {
    val initialGrid = Source.fromFile("input.txt").getLines().map(_.map(_ - '0').toArray).toArray
    val extendedGrid = extendGrid(initialGrid)
    println(dijkstra(extendedGrid))
  }

  def dijkstra(grid: Array[Array[Int]]): Int = {
    val pq = mutable.PriorityQueue.empty(Ordering.by[Position, Int](_.risk).reverse)
    pq.enqueue(Position(0, 0, 0))

    val rows = grid.length
    val cols = grid(0).length
    val dist = Array.fill(rows, cols)(Int.MaxValue)
    dist(0)(0) = 0

    val directions = Array((1, 0), (0, 1), (-1, 0), (0, -1))

    while (pq.nonEmpty) {
      val curr = pq.dequeue()
      if (curr.x == rows - 1 && curr.y == cols - 1) return curr.risk

      for ((dx, dy) <- directions) {
        val (nx, ny) = (curr.x + dx, curr.y + dy)
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

  def extendGrid(initialGrid: Array[Array[Int]]): Array[Array[Int]] = {
    val rows = initialGrid.length
    val cols = initialGrid(0).length
    val extendedGrid = Array.ofDim[Int](rows * 5, cols * 5)

    for (i <- 0 until rows * 5; j <- 0 until cols * 5) {
      val newRisk = (initialGrid(i % rows)(j % cols) + i / rows + j / cols - 1) % 9 + 1
      extendedGrid(i)(j) = newRisk
    }
    extendedGrid
  }
}
