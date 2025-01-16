
import scala.collection.mutable
import scala.io.Source

object Main {
  def canReach(grid: Array[Array[Boolean]]): Boolean = {
    val n = grid.length
    if (grid(0)(0) || grid(n - 1)(n - 1)) return false

    val dirs = Array(Array(1, 0), Array(-1, 0), Array(0, 1), Array(0, -1))
    val visited = Array.ofDim[Boolean](n, n)
    val q = mutable.Queue((0, 0))
    visited(0)(0) = true

    while (q.nonEmpty) {
      val (cx, cy) = q.dequeue()
      if (cx == n - 1 && cy == n - 1) return true

      for (d <- dirs) {
        val nx = cx + d(0)
        val ny = cy + d(1)
        if (nx >= 0 && ny >= 0 && nx < n && ny < n && !grid(ny)(nx) && !visited(ny)(nx)) {
          visited(ny)(nx) = true
          q.enqueue((nx, ny))
        }
      }
    }
    false
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("input.txt")
    val lines = source.getLines().toList
    source.close()

    val size = 71
    val grid = Array.ofDim[Boolean](size, size)

    for (line <- lines) {
      val Array(xStr, yStr) = line.split(",")
      val x = xStr.toInt
      val y = yStr.toInt
      if (x >= 0 && x < size && y >= 0 && y < size) {
        grid(y)(x) = true
      }
      if (!canReach(grid)) {
        println(s"$x,$y")
        return
      }
    }
    println("No cutoff found")
  }
}
