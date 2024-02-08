import scala.io.Source

object Day24 extends App {
  val input = Source.fromFile("input.txt").getLines.toList

  val directions = List("e", "se", "sw", "w", "nw", "ne")
  var blackTiles = Set.empty[(Int, Int)]

  def getTileCoordinates(direction: String): (Int, Int) = {
    var x = 0
    var y = 0

    var i = 0
    while (i < direction.length) {
      if (direction(i) == 'e') {
        x += 1
        i += 1
      } else if (direction(i) == 'w') {
        x -= 1
        i += 1
      } else if (direction(i) == 's') {
        if (direction(i + 1) == 'e') {
          x += 1
          y -= 1
        } else {
          y -= 1
        }
        i += 2
      } else if (direction(i) == 'n') {
        if (direction(i + 1) == 'w') {
          x -= 1
          y += 1
        } else {
          y += 1
        }
        i += 2
      }
    }

    (x, y)
  }

  for (direction <- input) {
    val tile = getTileCoordinates(direction)
    if (blackTiles.contains(tile)) {
      blackTiles -= tile
    } else {
      blackTiles += tile
    }
  }

  println(blackTiles.size)
}