import scala.io.Source

case class Point(x: Int, y: Int)

object TransparentOrigami {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val (points, folds) = lines.span(_.indexOf(',') >= 0)
    val pointSet = points.map { line =>
      val Array(x, y) = line.split(",").map(_.toInt)
      Point(x, y)
    }.toSet

    val foldInstructions = folds.drop(1).map { line =>
      val Array(_, axisValue) = line.split("fold along ")
      val Array(axis, value) = axisValue.split("=")
      (axis.head, value.toInt)
    }

    var currentPoints = pointSet
    for (instruction <- foldInstructions) {
      currentPoints = currentPoints.map { point =>
        if (instruction._1 == 'x' && point.x > instruction._2) {
          Point(instruction._2 - (point.x - instruction._2), point.y)
        } else if (instruction._1 == 'y' && point.y > instruction._2) {
          Point(point.x, instruction._2 - (point.y - instruction._2))
        } else {
          point
        }
      }.toSet
    }

    val maxX = currentPoints.map(_.x).max
    val maxY = currentPoints.map(_.y).max

    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        if (currentPoints.contains(Point(x, y))) {
          print("#")
        } else {
          print(" ")
        }
      }
      println()
    }
  }
}