import scala.io.Source

object HexEd {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().mkString.split(",").toList

    var x = 0
    var y = 0
    var z = 0
    var maxDistance = 0

    for (direction <- input) {
      direction match {
        case "n" => { y += 1; z -= 1 }
        case "ne" => { x += 1; z -= 1 }
        case "se" => { x += 1; y -= 1 }
        case "s" => { y -= 1; z += 1 }
        case "sw" => { x -= 1; z += 1 }
        case "nw" => { x -= 1; y += 1 }
      }

      val distance = (x.abs + y.abs + z.abs) / 2
      if (distance > maxDistance) {
        maxDistance = distance
      }
    }

    println(maxDistance)
  }
}