
import scala.io.Source
import scala.collection.mutable

object Main {
  val Open = '.'
  val Trees = '|'
  val Lumberyard = '#'
  val Size = 50

  def main(args: Array[String]): Unit = {
    val grid = readInput("input.txt")
    val seenStates = mutable.Map[String, Int]()
    var cycleStart = 0
    var cycleLength = 0

    var currentGrid = grid
    var minute = 0
    while (cycleLength == 0) {
      val state = gridToString(currentGrid)
      if (seenStates.contains(state)) {
        cycleStart = seenStates(state)
        cycleLength = minute - seenStates(state)
      } else {
        seenStates(state) = minute
        currentGrid = transform(currentGrid)
        minute += 1
      }
    }

    val remainingMinutes = (1000000000 - cycleStart) % cycleLength
    for (_ <- 0 until remainingMinutes) {
      currentGrid = transform(currentGrid)
    }

    val (wooded, lumberyards) = countResources(currentGrid)
    println(wooded * lumberyards)
  }

  def readInput(filename: String): Array[Array[Char]] = {
    Source.fromFile(filename).getLines().map(_.toArray).toArray
  }

  def transform(grid: Array[Array[Char]]): Array[Array[Char]] = {
    val newGrid = Array.ofDim[Char](grid.length, grid(0).length)
    for (i <- grid.indices; j <- grid(i).indices) {
      newGrid(i)(j) = nextAcreState(grid, i, j)
    }
    newGrid
  }

  def nextAcreState(grid: Array[Array[Char]], i: Int, j: Int): Char = {
    grid(i)(j) match {
      case Open => if (countAdjacent(grid, i, j, Trees) >= 3) Trees else Open
      case Trees => if (countAdjacent(grid, i, j, Lumberyard) >= 3) Lumberyard else Trees
      case Lumberyard => if (countAdjacent(grid, i, j, Lumberyard) >= 1 && countAdjacent(grid, i, j, Trees) >= 1) Lumberyard else Open
    }
  }

  def countAdjacent(grid: Array[Array[Char]], i: Int, j: Int, acreType: Char): Int = {
    var count = 0
    for (x <- -1 to 1; y <- -1 to 1) {
      if (x != 0 || y != 0) {
        val ni = i + x
        val nj = j + y
        if (ni >= 0 && ni < grid.length && nj >= 0 && nj < grid(0).length && grid(ni)(nj) == acreType) {
          count += 1
        }
      }
    }
    count
  }

  def countResources(grid: Array[Array[Char]]): (Int, Int) = {
    var wooded = 0
    var lumberyards = 0
    for (i <- grid.indices; j <- grid(i).indices) {
      grid(i)(j) match {
        case Trees => wooded += 1
        case Lumberyard => lumberyards += 1
        case _ =>
      }
    }
    (wooded, lumberyards)
  }

  def gridToString(grid: Array[Array[Char]]): String = {
    grid.map(_.mkString).mkString("\n")
  }
}
