
import scala.io.Source
import scala.collection.mutable

case class Coord(x: Int, y: Int) {
  def add(c: Coord): Coord = Coord(x + c.x, y + c.y)
  def isInBounds(width: Int, height: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}

case class Grid(width: Int, height: Int, data: mutable.Map[Coord, Char]) {
  def toStringGrid: String = {
    (for (y <- 0 until height) yield {
      (for (x <- 0 until width) yield data.getOrElse(Coord(x, y), '.')).mkString("")
    }).mkString("\n") + "\n"
  }
}

object RockSimulation {
  val Empty: Char = '.'
  val CubicRock: Char = '#'
  val RoundRock: Char = 'O'
  val Directions = Seq(Coord(0, -1), Coord(-1, 0), Coord(0, 1), Coord(1, 0))

  def buildGrid(input: Seq[String]): Grid = {
    val data = mutable.Map[Coord, Char]()
    for (y <- input.indices; x <- input(y).indices) {
      if (input(y)(x) != Empty) data(Coord(x, y)) = input(y)(x)
    }
    Grid(input.head.length, input.length, data)
  }

  def shiftSingleRock(grid: Grid, coord: Coord, dir: Coord): Unit = {
    if (grid.data.get(coord).contains(RoundRock)) {
      var current = coord
      var before = current.add(dir)
      while (before.isInBounds(grid.width, grid.height) && !grid.data.contains(before)) {
        grid.data(before) = RoundRock
        grid.data.remove(current)
        current = before
        before = before.add(dir)
      }
    }
  }

  def shiftRocks(grid: Grid, dir: Coord): Unit = {
    val range = if (dir == Coord(0, -1) || dir == Coord(-1, 0)) {
      (0 until grid.height).flatMap(y => (0 until grid.width).map(x => Coord(x, y)))
    } else {
      (grid.height - 1 to 0 by -1).flatMap(y => (grid.width - 1 to 0 by -1).map(x => Coord(x, y)))
    }
    range.foreach(shiftSingleRock(grid, _, dir))
  }

  def cycleRocks(grid: Grid): Unit = {
    Directions.foreach(shiftRocks(grid, _))
  }

  def calculateGridKey(grid: Grid): Int = {
    grid.data.collect {
      case (coord, RoundRock) => coord.x + coord.y * grid.width
    }.sum
  }

  def calculateLoad(grid: Grid): Int = {
    grid.data.collect {
      case (coord, RoundRock) => grid.height - coord.y
    }.sum
  }

  def solve(input: Seq[String]): Int = {
    val numCycles = 1000000000
    val grid = buildGrid(input)
    val cache = mutable.Map[Int, Int]()

    for (i <- 0 until numCycles) {
      val gridKey = calculateGridKey(grid)
      cache.get(gridKey) match {
        case Some(iStartCycle) =>
          val remainingCycles = (numCycles - iStartCycle) % (i - iStartCycle)
          for (_ <- 0 until remainingCycles) cycleRocks(grid)
          return calculateLoad(grid)
        case None =>
          cache(gridKey) = i
      }
      cycleRocks(grid)
    }
    calculateLoad(grid)
  }

  def readFile(fileName: String): Seq[String] = {
    Source.fromFile(fileName).getLines().toSeq
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
