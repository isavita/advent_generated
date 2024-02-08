
import scala.io.Source

object Solution extends App {
  val grid = readInput("input.txt")
  var step = 0

  while (true) {
    step += 1
    val flashes = simulateStep(grid)
    if (flashes == 100) {
      println(step)
      System.exit(0)
    }
  }

  def readInput(filename: String): Array[Array[Int]] = {
    val bufferedSource = Source.fromFile(filename)
    val grid = bufferedSource.getLines.map(_.map(_.asDigit).toArray).toArray
    bufferedSource.close
    grid
  }

  def simulateStep(grid: Array[Array[Int]]): Int = {
    var flashes = 0
    var flashed = scala.collection.mutable.Set[(Int, Int)]()

    for (y <- grid.indices; x <- grid(y).indices) {
      grid(y)(x) += 1
    }

    for (y <- grid.indices; x <- grid(y).indices) {
      if (grid(y)(x) > 9) {
        flashes += flash(grid, x, y, flashed)
      }
    }

    for ((x, y) <- flashed) {
      grid(y)(x) = 0
    }

    flashes
  }

  def flash(grid: Array[Array[Int]], x: Int, y: Int, flashed: scala.collection.mutable.Set[(Int, Int)]): Int = {
    if (flashed.contains((x, y))) {
      return 0
    }

    flashed.add((x, y))
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
