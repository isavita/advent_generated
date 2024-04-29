import scala.io.Source
import scala.collection.mutable

object Main {
  case class Coordinate(q: Int, r: Int)

  val directions = Map(
    "e" -> Coordinate(1, 0),
    "se" -> Coordinate(0, 1),
    "sw" -> Coordinate(-1, 1),
    "w" -> Coordinate(-1, 0),
    "nw" -> Coordinate(0, -1),
    "ne" -> Coordinate(1, -1)
  )

  def getNeighbors(tile: Coordinate): Seq[Coordinate] = {
    directions.map { case (_, dir) => Coordinate(tile.q + dir.q, tile.r + dir.r) }.toSeq
  }

  def main(args: Array[String]): Unit = {
    val blackTiles = mutable.Map[Coordinate, Boolean]()
    for (line <- Source.fromFile("input.txt").getLines()) {
      var coord = Coordinate(0, 0)
      var i = 0
      while (i < line.length) {
        var dir = ""
        if (line(i) == 'e' || line(i) == 'w') {
          dir = line(i).toString
        } else {
          dir = line.substring(i, i + 2)
          i += 1
        }
        val move = directions(dir)
        coord = Coordinate(coord.q + move.q, coord.r + move.r)
        i += 1
      }
      blackTiles(coord) = !blackTiles.getOrElse(coord, false)
    }

    for (_ <- 0 to 99) {
      val tilesToCheck = mutable.Map[Coordinate, Boolean]()
      for ((tile, _) <- blackTiles) {
        tilesToCheck(tile) = true
        for (neighbor <- getNeighbors(tile)) {
          tilesToCheck(neighbor) = true
        }
      }

      val newBlackTiles = mutable.Map[Coordinate, Boolean]()
      for ((tile, _) <- tilesToCheck) {
        var blackNeighborCount = 0
        for (neighbor <- getNeighbors(tile)) {
          if (blackTiles.getOrElse(neighbor, false)) {
            blackNeighborCount += 1
          }
        }
        if (blackTiles.getOrElse(tile, false) && (blackNeighborCount == 1 || blackNeighborCount == 2)) {
          newBlackTiles(tile) = true
        } else if (!blackTiles.getOrElse(tile, false) && blackNeighborCount == 2) {
          newBlackTiles(tile) = true
        }
      }

      blackTiles.clear()
      blackTiles ++= newBlackTiles
    }

    println(blackTiles.size)
  }
}