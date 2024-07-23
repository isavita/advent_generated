
import scala.io.Source

case class Coord(x: Int, y: Int) {
  def add(c: Coord): Coord = Coord(x + c.x, y + c.y)
  def isInBounds(width: Int, height: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}

case class Grid(width: Int, height: Int, data: Map[Coord, Char]) {
  def toStringGrid: String = {
    (for (y <- 0 until height) yield {
      (for (x <- 0 until width) yield {
        data.getOrElse(Coord(x, y), '.')
      }).mkString("")
    }).mkString("\n") + "\n"
  }
}

object RockSimulation {
  val Empty: Char = '.'
  val CubicRock: Char = '#'
  val RoundRock: Char = 'O'
  
  val directions = Map(
    "North" -> Coord(0, -1),
    "West" -> Coord(-1, 0),
    "South" -> Coord(0, 1),
    "East" -> Coord(1, 0)
  )

  def buildGrid(input: Seq[String]): Grid = {
    val data = for {
      (line, y) <- input.zipWithIndex
      (char, x) <- line.zipWithIndex if char != Empty
    } yield Coord(x, y) -> char
    Grid(input.head.length, input.length, data.toMap)
  }

  def shiftSingleRock(grid: Grid, coord: Coord, dir: Coord): Grid = {
    if (grid.data.get(coord).contains(RoundRock)) {
      var current = coord
      var before = coord.add(dir)
      var newData = grid.data

      while (before.isInBounds(grid.width, grid.height) && !newData.contains(before)) {
        newData = newData.updated(before, RoundRock).removed(current)
        current = before
        before = before.add(dir)
      }
      grid.copy(data = newData)
    } else grid
  }

  def shiftRocks(grid: Grid, dir: Coord): Grid = {
    val range = if (dir == directions("North") || dir == directions("West")) {
      (0 until grid.width).flatMap(x => (0 until grid.height).map(y => Coord(x, y)))
    } else {
      (grid.width - 1 to 0 by -1).flatMap(x => (grid.height - 1 to 0 by -1).map(y => Coord(x, y)))
    }
    range.foldLeft(grid)((g, coord) => shiftSingleRock(g, coord, dir))
  }

  def calculateLoad(grid: Grid): Int = {
    (for {
      x <- 0 until grid.width
      y <- 0 until grid.height
      if grid.data.get(Coord(x, y)).contains(RoundRock)
    } yield grid.height - y).sum
  }

  def solve(input: Seq[String]): Int = {
    val grid = buildGrid(input)
    val shiftedGrid = shiftRocks(grid, directions("North"))
    calculateLoad(shiftedGrid)
  }

  def readFile(fileName: String): Seq[String] = {
    Source.fromFile(fileName).getLines().toSeq
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
