object TransparentOrigami {
  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val lines = io.Source.fromFile("input.txt").getLines().toList
    val points = lines.takeWhile(_.nonEmpty).map { line =>
      val Array(x, y) = line.split(",").map(_.toInt)
      Point(x, y)
    }
    val folds = lines.dropWhile(_.nonEmpty).drop(1).map { line =>
      val foldInstruction = line.split("fold along ") (1)
      val Array(axis, value) = foldInstruction.split("=")
      (axis, value.toInt)
    }

    val (axis, value) = folds.head
    val newPoints = points.map { point =>
      if (axis == "x" && point.x > value) {
        Point(2 * value - point.x, point.y)
      } else if (axis == "y" && point.y > value) {
        Point(point.x, 2 * value - point.y)
      } else {
        point
      }
    }

    val visibleDots = newPoints.toSet.size
    println(s"There are $visibleDots visible dots after the first fold.")
  }
}