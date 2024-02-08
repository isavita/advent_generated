
import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val grid = readInput("input.txt")

    var totalFlashes = 0
    for (_ <- 0 until 100) {
      totalFlashes += simulateStep(grid)
    }

    println(totalFlashes)
  }

  def readInput(filename: String): Array[Array[Int]] = {
    val bufferedSource = Source.fromFile(filename)
    val grid = bufferedSource.getLines.map(_.map(_.asDigit).toArray).toArray
    bufferedSource.close()
    grid
  }

  def simulateStep(grid: Array[Array[Int]]): Int = {
    var flashes = 0
    val flashed = scala.collection.mutable.Map[(Int, Int), Boolean]()

    for (y <- grid.indices; x <- grid(y).indices) {
      grid(y)(x) += 1
    }

    for (y <- grid.indices; x <- grid(y).indices) {
      if (grid(y)(x) > 9) {
        flashes += flash(grid, x, y, flashed)
      }
    }

    for ((x, y) <- flashed.keys) {
      grid(y)(x) = 0
    }

    flashes
  }

  def flash(grid: Array[Array[Int]], x: Int, y: Int, flashed: scala.collection.mutable.Map[(Int, Int), Boolean]): Int = {
    if (flashed.contains((x, y))) {
      return 0
    }

    flashed((x, y)) = true
    var flashes = 1
    val directions = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

    for ((dx, dy) <- directions) {
      val newX = x + dx
      val newY = y + dy
      if (newX >= 0 && newX < grid(0).length && newY >= 0 && newY < grid.length) {
        grid(newY)(newX) += 1
        if (grid(newY)(newX) > 9) {
          flashes += flash(grid, newX, newY, flashed)
        }
      }
    }

    flashes
  }
}
