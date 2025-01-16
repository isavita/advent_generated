
import scala.io.Source
import scala.math.sqrt
import scala.collection.mutable
import scala.util.Using

object Main {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromFile("input.txt"))(_.mkString).get
    val ans = solve(input)
    println(ans)
  }

  def solve(input: String): Int = {
    val tiles = parseTilesFromInput(input)
    val edgeSize = sqrt(tiles.length).toInt

    val assembledTiles = backtrackAssemble(tiles, edgeSize)

    val processedTiles = assembledTiles.map(_.map(tile =>
      tile.copy(contents = removeBordersFromGrid(tile.contents))
    ))

    val image = (0 until edgeSize).flatMap(bigRow =>
      (0 until processedTiles(0)(0).contents.length).map(subRow =>
        (0 until edgeSize).flatMap(bigCol =>
          processedTiles(bigRow)(bigCol).contents(subRow)
        ).toArray
      )
    ).toArray

    val (finalImage, _) = allGridOrientations(image).find(findMonsterCoords(_).nonEmpty).map(img => (img, findMonsterCoords(img))).getOrElse((image, List()))

    val markedImage = finalImage.map(row => row.map {
      case "#" => "#"
      case _ => "."
    })

    findMonsterCoords(finalImage).foreach { case (r, c) =>
      markedImage(r)(c) = "O"
    }

    markedImage.flatten.count(_ == "#")
  }

  case class Tile(contents: Array[Array[String]], id: Int)

  def parseTilesFromInput(input: String): Array[Tile] = {
    input.split("\n\n").map { block =>
      val lines = block.split("\n")
      val tileID = lines(0).drop(5).dropRight(1).toInt
      val contents = lines.drop(1).map(_.split("").toArray).toArray
      Tile(contents, tileID)
    }
  }

  def backtrackAssemble(tiles: Array[Tile], edgeSize: Int): Array[Array[Tile]] = {
    val assembledTiles = Array.ofDim[Tile](edgeSize, edgeSize)
    val usedIndices = mutable.Set[Int]()

    def recurse(row: Int, col: Int): Boolean = {
      if (row == edgeSize) return true

      val nextRow = if (col == edgeSize - 1) row + 1 else row
      val nextCol = if (col == edgeSize - 1) 0 else col + 1

      if (assembledTiles(row)(col) != null) return recurse(nextRow, nextCol)

      for ((tile, i) <- tiles.zipWithIndex if !usedIndices.contains(i)) {
        for (orientation <- allGridOrientations(tile.contents)) {
          val validPlacement = (row == 0 || getRow(orientation, true) == getRow(assembledTiles(row - 1)(col).contents, false)) &&
            (col == 0 || getCol(orientation, true) == getCol(assembledTiles(row)(col - 1).contents, false))

          if (validPlacement) {
            assembledTiles(row)(col) = tile.copy(contents = orientation)
            usedIndices.add(i)
            if (recurse(nextRow, nextCol)) return true
            usedIndices.remove(i)
            assembledTiles(row)(col) = null
          }
        }
      }
      false
    }

    if (recurse(0, 0)) assembledTiles else null
  }

  def getCol(grid: Array[Array[String]], firstCol: Boolean): String = {
    grid.map(row => if (firstCol) row(0) else row.last).mkString
  }

  def getRow(grid: Array[Array[String]], firstRow: Boolean): String = {
    if (firstRow) grid(0).mkString else grid.last.mkString
  }

  def removeBordersFromGrid(grid: Array[Array[String]]): Array[Array[String]] = {
    grid.slice(1, grid.length - 1).map(row => row.slice(1, row.length - 1))
  }

  val monster =
    """                  #
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin

  def findMonsterCoords(image: Array[Array[String]]): List[(Int, Int)] = {
    val monsterOffsets = monster.split("\n").zipWithIndex.flatMap { case (line, r) =>
      line.zipWithIndex.filter(_._1 == '#').map { case (_, c) => (r, c) }
    }.toList

    val monsterHeight = monster.split("\n").length
    val monsterLength = monster.split("\n").head.length

    val monsterStartingCoords = for {
      r <- 0 until image.length - monsterHeight + 1
      c <- 0 until image(0).length - monsterLength + 1
      if monsterOffsets.forall { case (dr, dc) => image(r + dr)(c + dc) == "#" }
    } yield (r, c)

    monsterStartingCoords.flatMap { case (r, c) =>
      monsterOffsets.map { case (dr, dc) => (r + dr, c + dc) }
    }.toList
  }

  def allGridOrientations(grid: Array[Array[String]]): List[Array[Array[String]]] = {
    val orientations = mutable.ListBuffer(grid)
    var current = grid
    for (_ <- 0 until 3) {
      current = rotateStringGrid(current)
      orientations += current
    }
    for (i <- 0 until 4) {
      orientations += mirrorStringGrid(orientations(i))
    }
    orientations.toList
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
