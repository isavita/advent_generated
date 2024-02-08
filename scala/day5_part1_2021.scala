import scala.io.Source
import scala.collection.mutable.Map

case class Point(x: Int, y: Int)

object Solution {
  def main(args: Array[String]): Unit = {
    val grid = Map[Point, Int]()

    val lines = Source.fromFile("input.txt").getLines().toList
    for (line <- lines) {
      val Array(startCoords, endCoords) = line.split(" -> ")
      val Array(x1, y1) = startCoords.split(",")
      val Array(x2, y2) = endCoords.split(",")

      if (x1.toInt == x2.toInt) {
        val startY = y1.toInt min y2.toInt
        val endY = y1.toInt max y2.toInt
        for (y <- startY to endY) {
          grid(Point(x1.toInt, y)) = grid.getOrElse(Point(x1.toInt, y), 0) + 1
        }
      } else if (y1.toInt == y2.toInt) {
        val startX = x1.toInt min x2.toInt
        val endX = x1.toInt max x2.toInt
        for (x <- startX to endX) {
          grid(Point(x, y1.toInt)) = grid.getOrElse(Point(x, y1.toInt), 0) + 1
        }
      }
    }

    val overlapCount = grid.count { case (_, v) => v > 1 }
    println(overlapCount)
  }
}