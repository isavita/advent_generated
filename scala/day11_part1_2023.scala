import scala.io.Source
import scala.collection.mutable

object Main {
  type Coord = (Int, Int)

  case class Grid(width: Int, height: Int, data: mutable.Map[Coord, Byte]) {
    def toString(empty: Byte): String = {
      val builder = new StringBuilder
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val coord = (x, y)
          if (data.contains(coord)) {
            builder.append(data(coord))
          } else {
            builder.append(empty)
          }
        }
        builder.append('\n')
      }
      builder.toString
    }

    def getEmptyRows: Seq[Int] = {
      for (y <- 0 until height if (0 until width).forall(x => !data.contains((x, y)))) yield y
    }

    def getEmptyCols: Seq[Int] = {
      for (x <- 0 until width if (0 until height).forall(y => !data.contains((x, y)))) yield x
    }
  }

  def calculateOffsets(emptyIndexes: Seq[Int], bound: Int): Seq[Int] = {
    val offsets = Array.fill(bound)(0)
    for (idx <- emptyIndexes) {
      for (i <- idx + 1 until bound) {
        offsets(i) += 1
      }
    }
    offsets
  }

  def expandGrid(grid: Grid, expansionFactor: Int): Grid = {
    val emptyCols = grid.getEmptyCols
    val emptyRows = grid.getEmptyRows
    val numLinesToAdd = expansionFactor - 1

    val newGrid = Grid(
      width = grid.width + emptyCols.length * numLinesToAdd,
      height = grid.height + emptyRows.length * numLinesToAdd,
      data = mutable.Map[Coord, Byte]()
    )

    val dXs = calculateOffsets(emptyCols, grid.width)
    val dYs = calculateOffsets(emptyRows, grid.height)

    for (y <- 0 until grid.height) {
      for (x <- 0 until grid.width) {
        val coord = (x, y)
        if (grid.data.contains(coord)) {
          val newCoord = (x + dXs(x) * numLinesToAdd, y + dYs(y) * numLinesToAdd)
          newGrid.data += newCoord -> grid.data(coord)
        }
      }
    }

    newGrid
  }

  def abs(x: Int): Int = if (x < 0) -x else x

  def calculateLength(grid: Grid, c1: Coord, c2: Coord): Int = {
    val dX = abs(c2._1 - c1._1)
    val dY = abs(c2._2 - c1._2)
    dX + dY
  }

  def solve(input: Seq[String]): Int = {
    val grid = buildGrid(input, '.'.toByte)
    val expandedGrid = expandGrid(grid, 2)

    var res = 0
    val alreadySeen = mutable.Set[Coord]()
    for (coord1 <- expandedGrid.data.keys) {
      for (coord2 <- alreadySeen) {
        res += calculateLength(expandedGrid, coord1, coord2)
      }
      alreadySeen += coord1
    }

    res
  }

  def buildGrid(input: Seq[String], empty: Byte): Grid = {
    val width = input.head.length
    val height = input.length
    val data = mutable.Map[Coord, Byte]()

    for ((line, y) <- input.zipWithIndex) {
      for ((char, x) <- line.zipWithIndex) {
        if (char != empty) {
          data += ((x, y) -> char.toByte)
        }
      }
    }

    Grid(width, height, data)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toSeq
    println(solve(input))
  }
}