
import scala.io.Source

object SeaCucumber {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toArray
    val result = simulateSeaCucumbers(input)
    println(result)
  }

  def simulateSeaCucumbers(grid: Array[String]): Int = {
    var currentGrid = grid
    var steps = 0

    while (true) {
      val (eastMoved, eastGrid) = moveEastCucumbers(currentGrid)
      val (southMoved, southGrid) = moveSouthCucumbers(eastGrid)

      steps += 1

      if (!eastMoved && !southMoved) {
        return steps
      }

      currentGrid = southGrid
    }

    steps
  }

  def moveEastCucumbers(grid: Array[String]): (Boolean, Array[String]) = {
    val rows = grid.length
    val cols = grid(0).length
    val newGrid = Array.fill(rows)(new StringBuilder("." * cols))
    var moved = false

    for (r <- 0 until rows; c <- 0 until cols) {
      if (grid(r)(c) == '>') {
        val nextCol = (c + 1) % cols
        if (grid(r)(nextCol) == '.') {
          newGrid(r)(nextCol) = '>'
          moved = true
        } else {
          newGrid(r)(c) = '>'
        }
      } else if (grid(r)(c) == 'v') {
        newGrid(r)(c) = 'v'
      }
    }

    (moved, newGrid.map(_.toString))
  }

  def moveSouthCucumbers(grid: Array[String]): (Boolean, Array[String]) = {
    val rows = grid.length
    val cols = grid(0).length
    val newGrid = Array.fill(rows)(new StringBuilder("." * cols))
    var moved = false

    for (r <- 0 until rows; c <- 0 until cols) {
      if (grid(r)(c) == 'v') {
        val nextRow = (r + 1) % rows
        if (grid(nextRow)(c) == '.') {
          newGrid(nextRow)(c) = 'v'
          moved = true
        } else {
          newGrid(r)(c) = 'v'
        }
      } else if (grid(r)(c) == '>') {
        newGrid(r)(c) = '>'
      }
    }

    (moved, newGrid.map(_.toString))
  }
}
