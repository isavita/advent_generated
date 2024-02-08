
object Day6 extends App {
  val coordinates = scala.io.Source.fromFile("input.txt").getLines().map { line =>
    val Array(x, y) = line.split(", ").map(_.toInt)
    (x, y)
  }.toList

  val minX = coordinates.map(_._1).min
  val maxX = coordinates.map(_._1).max
  val minY = coordinates.map(_._2).min
  val maxY = coordinates.map(_._2).max

  def manhattanDistance(p1: (Int, Int), p2: (Int, Int)): Int = {
    Math.abs(p1._1 - p2._1) + Math.abs(p1._2 - p2._2)
  }

  def closestCoordinate(point: (Int, Int)): Option[Int] = {
    val distances = coordinates.zipWithIndex.map { case (coord, index) =>
      (index, manhattanDistance(coord, point))
    }
    val minDistance = distances.minBy(_._2)._2
    val closestCoords = distances.filter(_._2 == minDistance)
    if (closestCoords.length == 1) Some(closestCoords.head._1) else None
  }

  val areaSizes = Array.fill(coordinates.length)(0)
  var infiniteAreas = Set[Int]()

  for (x <- minX to maxX; y <- minY to maxY) {
    closestCoordinate((x, y)) match {
      case Some(index) =>
        if (x == minX || x == maxX || y == minY || y == maxY) {
          infiniteAreas += index
        } else {
          areaSizes(index) += 1
        }
      case None =>
    }
  }

  val largestArea = areaSizes.zipWithIndex.filterNot { case (_, index) =>
    infiniteAreas.contains(index)
  }.maxBy(_._1)._1

  println(largestArea)
}
