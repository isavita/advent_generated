import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Main {
  val gridSize = 100
  val steps = 100

  def countOnNeighbors(grid: Array[Array[Boolean]], x: Int, y: Int): Int = {
    var on = 0
    for {
      dx <- -1 to 1
      dy <- -1 to 1
      if dx != 0 || dy != 0
    } {
      val nx = x + dx
      val ny = y + dy
      if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid(nx)(ny)) {
        on += 1
      }
    }
    on
  }

  def step(grid: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val newGrid = Array.fill(gridSize, gridSize)(false)
    for {
      x <- 0 until gridSize
      y <- 0 until gridSize
    } {
      val onNeighbors = countOnNeighbors(grid, x, y)
      if (grid(x)(y)) {
        newGrid(x)(y) = onNeighbors == 2 || onNeighbors == 3
      } else {
        newGrid(x)(y) = onNeighbors == 3
      }
    }
    newGrid(0)(0) = true
    newGrid(0)(gridSize - 1) = true
    newGrid(gridSize - 1)(0) = true
    newGrid(gridSize - 1)(gridSize - 1) = true
    newGrid
  }

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input.txt")
    var grid = Array.fill(gridSize, gridSize)(false)
    var y = 0
    for (line <- file.getLines) {
      for ((c, x) <- line.zipWithIndex) {
        grid(x)(y) = c == '#'
      }
      y += 1
    }
    file.close()

    grid(0)(0) = true
    grid(0)(gridSize - 1) = true
    grid(gridSize - 1)(0) = true
    grid(gridSize - 1)(gridSize - 1) = true

    for (_ <- 0 until steps) {
      grid = step(grid)
    }

    var onCount = 0
    for (row <- grid; light <- row) {
      if (light) {
        onCount += 1
      }
    }

    println(onCount)
  }
}