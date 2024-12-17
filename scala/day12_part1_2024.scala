
import scala.io.Source
import scala.collection.mutable

object Solution {
  def main(args: Array[String]): Unit = {
    val grid = Source.fromFile("input.txt").getLines().toArray
    println(solve(grid))
  }

  def solve(grid: Array[String]): Int = {
    val rows = grid.length
    if (rows == 0) return 0
    val cols = grid(0).length
    val visited = Array.ofDim[Boolean](rows, cols)
    var totalPrice = 0

    for (r <- 0 until rows; c <- 0 until cols) {
      if (!visited(r)(c)) {
        val (area, perimeter) = calculateRegion(grid, r, c, visited)
        totalPrice += area * perimeter
      }
    }
    totalPrice
  }

  def calculateRegion(grid: Array[String], row: Int, col: Int, visited: Array[Array[Boolean]]): (Int, Int) = {
    val rows = grid.length
    val cols = grid(0).length
    val char = grid(row)(col)
    var area = 0
    var perimeter = 0
    val queue = mutable.Queue((row, col))
    visited(row)(col) = true

    while (queue.nonEmpty) {
      val (r, c) = queue.dequeue()
      area += 1
      val isBorder = r == 0 || r == rows - 1 || c == 0 || c == cols - 1

      def checkNeighbor(nr: Int, nc: Int): Unit = {
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
          if (grid(nr)(nc) != char) perimeter += 1
          else if (!visited(nr)(nc)) {
            queue.enqueue((nr, nc))
            visited(nr)(nc) = true
          }
        } else if (isBorder) perimeter += 1
      }

      checkNeighbor(r - 1, c)
      checkNeighbor(r + 1, c)
      checkNeighbor(r, c - 1)
      checkNeighbor(r, c + 1)
    }
    (area, perimeter)
  }
}
