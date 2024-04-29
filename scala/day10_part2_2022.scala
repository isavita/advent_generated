import scala.io.Source
import scala.math.abs
import scala.collection.mutable

case class Point(x: Int, y: Int)

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toSeq
    var x = Seq(1)
    for (line <- lines) {
      line match {
        case "noop" => x = x :+ x.last
        case _ => {
          val n = line.substring(5).toInt
          x = x :+ x.last :+ (x.last + n)
        }
      }
    }

    val grid = mutable.Map[Point, Unit]()
    for (i <- x.indices) {
      val crtx = i % 40
      val crty = i / 40
      if (abs(crtx - x(i)) <= 1) {
        grid += Point(crtx, crty) -> ()
      } else {
        grid -= Point(crtx, crty)
      }
    }

    for (y <- 0 until 6) {
      for (x <- 0 until 40) {
        if (grid.contains(Point(x, y))) {
          print("#")
        } else {
          print(".")
        }
      }
      println()
    }
  }
}