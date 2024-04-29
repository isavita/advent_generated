import scala.io.Source

object LavaDroplet {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().map { line =>
      val Array(x, y, z) = line.split(",").map(_.toInt)
      (x, y, z)
    }.toSet

    val adjacentDirections = List(
      (-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)
    )

    var surfaceArea = 0

    for {
      (x, y, z) <- input
      (dx, dy, dz) <- adjacentDirections
      adjacent = (x + dx, y + dy, z + dz)
      if !input.contains(adjacent)
    } surfaceArea += 1

    println(s"Surface area: $surfaceArea")
  }
}