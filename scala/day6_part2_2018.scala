
object Main extends App {
  val coordinates = scala.io.Source.fromFile("input.txt").getLines.map { line =>
    val Array(x, y) = line.split(", ").map(_.toInt)
    (x, y)
  }.toList

  def manhattanDistance(a: (Int, Int), b: (Int, Int)): Int = {
    math.abs(a._1 - b._1) + math.abs(a._2 - b._2)
  }

  val minX = coordinates.minBy(_._1)._1
  val maxX = coordinates.maxBy(_._1)._1
  val minY = coordinates.minBy(_._2)._2
  val maxY = coordinates.maxBy(_._2)._2

  val areaSizes = Array.ofDim[Int](coordinates.length)
  var regionSize = 0

  for {
    x <- minX to maxX
    y <- minY to maxY
  } {
    val totalDistance = coordinates.map(coord => manhattanDistance(coord, (x, y))).sum
    if (totalDistance < 10000) regionSize += 1

    val closest = coordinates.zipWithIndex.minBy { case (coord, _) =>
      manhattanDistance(coord, (x, y))
    }

    if (closest._1._1 != x && closest._1._2 != y) {
      areaSizes(closest._2) += 1
    }
  }

  val largestArea = areaSizes.max
  println(largestArea)
  println(regionSize)
}
