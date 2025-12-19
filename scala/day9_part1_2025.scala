
import scala.io.Source
import scala.math.abs

case class Point(x: Int, y: Int)

object Main extends App {
  val points = Source.fromFile("input.txt")
    .getLines()
    .map(_.trim)
    .filter(_.nonEmpty)
    .flatMap { line =>
      line.split(",") match {
        case Array(a, b) =>
          try {
            Some(Point(a.toInt, b.toInt))
          } catch {
            case _: NumberFormatException => None
          }
        case _ => None
      }
    }
    .toVector

  var maxArea: Long = 0L
  var i = 0
  while (i < points.length) {
    var j = i
    while (j < points.length) {
      val dx = abs(points(i).x - points(j).x).toLong + 1
      val dy = abs(points(i).y - points(j).y).toLong + 1
      val area = dx * dy
      if (area > maxArea) maxArea = area
      j += 1
    }
    i += 1
  }

  println(s"Largest area: $maxArea")
}
