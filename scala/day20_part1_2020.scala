
import scala.io.Source
import scala.math.sqrt
import scala.util.Using

object Main {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromFile("input.txt"))(_.mkString).get
    val ans = solve(input)
    println(ans)
  }

  def solve(input: String): Long = {
    val tiles = parseTilesFromInput(input)
    val edgeSize = sqrt(tiles.length).toInt
    val assembledTiles = backtrackAssemble(tiles, Array.ofDim[Tile](edgeSize, edgeSize), Set.empty)

    val product = assembledTiles(0)(0).id.toLong *
      assembledTiles(0)(edgeSize - 1).id.toLong *
      assembledTiles(edgeSize - 1)(0).id.toLong *
      assembledTiles(edgeSize - 1)(edgeSize - 1).id.toLong
    product
  }

  case class Tile(contents: Array[Array[String]], id: Int)

  def parseTilesFromInput(input: String): Array[Tile] = {
    input.split("\n\n").map { block =>
      val lines = block.split("\n")
      val tileID = lines(0).drop(5).dropRight(1).toInt
      val contents = lines.tail.map(_.split("").toArray)
      Tile(contents, tileID)
    }
  }

  def backtrackAssemble(tiles: Array[Tile], assembledTiles: Array[Array[Tile]], usedIndices: Set[Int]): Array[Array[Tile]] = {
    val edgeSize = sqrt(tiles.length).toInt

    def recurse(row: Int, col: Int, currentUsedIndices: Set[Int]): Array[Array[Tile]] = {
      if (row == edgeSize) return assembledTiles
      val nextRow = if (col == edgeSize - 1) row + 1 else row
      val nextCol = if (col == edgeSize - 1) 0 else col + 1

      if (assembledTiles(row)(col) != null) {
        return recurse(nextRow, nextCol, currentUsedIndices)
      }

      for (i <- tiles.indices) {
        if (!currentUsedIndices.contains(i)) {
          for (opt <- allGridOrientations(tiles(i).contents)) {
            if (row != 0) {
              val currentTopRow = getRow(opt, firstRow = true)
              val bottomOfAbove = getRow(assembledTiles(row - 1)(col).contents, firstRow = false)
              if (currentTopRow != bottomOfAbove) {
                
              } else {
                if (col != 0) {
                  val currentLeftCol = getCol(opt, firstCol = true)
                  val rightColOfLeft = getCol(assembledTiles(row)(col - 1).contents, firstCol = false)
                  if (currentLeftCol != rightColOfLeft) {
                    
                  } else {
                    assembledTiles(row)(col) = tiles(i).copy(contents = opt)
                    val result = recurse(nextRow, nextCol, currentUsedIndices + i)
                    if (result != null) return result
                    assembledTiles(row)(col) = null
                  }
                } else {
                  assembledTiles(row)(col) = tiles(i).copy(contents = opt)
                  val result = recurse(nextRow, nextCol, currentUsedIndices + i)
                  if (result != null) return result
                  assembledTiles(row)(col) = null
                }
              }
            } else {
              if (col != 0) {
                val currentLeftCol = getCol(opt, firstCol = true)
                val rightColOfLeft = getCol(assembledTiles(row)(col - 1).contents, firstCol = false)
                if (currentLeftCol != rightColOfLeft) {
                  
                } else {
                  assembledTiles(row)(col) = tiles(i).copy(contents = opt)
                  val result = recurse(nextRow, nextCol, currentUsedIndices + i)
                  if (result != null) return result
                  assembledTiles(row)(col) = null
                }
              } else {
                assembledTiles(row)(col) = tiles(i).copy(contents = opt)
                val result = recurse(nextRow, nextCol, currentUsedIndices + i)
                if (result != null) return result
                assembledTiles(row)(col) = null
              }
            }
          }
        }
      }
      null
    }

    recurse(0, 0, usedIndices)
  }

  def getCol(grid: Array[Array[String]], firstCol: Boolean): String = {
    val sb = new StringBuilder
    for (i <- grid.indices) {
      sb.append(if (firstCol) grid(i)(0) else grid(i)(grid(0).length - 1))
    }
    sb.toString
  }

  def getRow(grid: Array[Array[String]], firstRow: Boolean): String = {
    val sb = new StringBuilder
    for (i <- grid(0).indices) {
      sb.append(if (firstRow) grid(0)(i) else grid(grid.length - 1)(i))
    }
    sb.toString
  }

  def removeBordersFromGrid(grid: Array[Array[String]]): Array[Array[String]] = {
    grid.slice(1, grid.length - 1).map(row => row.slice(1, row.length - 1))
  }

  val monster =
    """                  #
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin

  def findMonsterCoords(image: Array[Array[String]]): Array[(Int, Int)] = {
    val monsterOffsets = monster.split("\n").zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.filter(_._1 == '#').map { case (_, c) => (r, c) }
    }.toArray
    val monsterHeight = monster.split("\n").length
    val monsterLength = monster.split("\n")(0).length

    val monsterStartingCoords = for {
      r <- 0 until image.length - monsterHeight + 1
      c <- 0 until image(0).length - monsterLength + 1
      if monsterOffsets.forall { case (dr, dc) => image(r + dr)(c + dc) == "#" }
    } yield (r, c)

    monsterStartingCoords.flatMap { case (r, c) =>
      monsterOffsets.map { case (dr, dc) => (r + dr, c + dc) }
    }.toArray
  }

  def allGridOrientations(grid: Array[Array[String]]): Array[Array[Array[String]]] = {
    val orientations = Array.newBuilder[Array[Array[String]]]
    var current = grid
    orientations += current
    for (_ <- 0 until 3) {
      current = rotateStringGrid(current)
      orientations += current
    }
    for (i <- 0 until 4) {
      orientations += mirrorStringGrid(orientations.result()(i))
    }
    orientations.result()
  }

  def rotateStringGrid(grid: Array[Array[String]]): Array[Array[String]] = {
    val rows = grid.length
    val cols = grid(0).length
    val rotated = Array.ofDim[String](cols, rows)
    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        rotated(cols - 1 - j)(i) = grid(i)(j)
      }
    }
    rotated
  }

  def mirrorStringGrid(grid: Array[Array[String]]): Array[Array[String]] = {
    grid.map(_.reverse)
  }
}
