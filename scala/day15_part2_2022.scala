
import scala.io.Source
import scala.util.Using

object Main {
  case class Point(x: Int, y: Int)
  case class Sensor(pos: Point, beacon: Point, dist: Int)

  def manhattan(p1: Point, p2: Point): Int =
    (p1.x - p2.x).abs + (p1.y - p2.y).abs

  def distress(sensors: Seq[Sensor], maxCoord: Int): Long = {
    for (x <- 0 to maxCoord) {
      var y = 0
      while (y <= maxCoord) {
        val p = Point(x, y)
        var detected = false
        var skip = 0
        for (s <- sensors) {
          if (manhattan(s.pos, p) <= s.dist) {
            detected = true
            val dist = s.dist - (s.pos.x - x).abs
            skip = skip max (dist + s.pos.y - y)
          }
        }
        if (!detected) {
          return x.toLong * 4000000L + y.toLong
        }
        y += skip max 1
      }
    }
    -1L
  }

  def main(args: Array[String]): Unit = {
    val sensors = Using(Source.fromFile("input.txt")) { source =>
      source.getLines().map { line =>
        val parts = line.split("[=,:]").map(_.trim)
        val sx = parts(1).toInt
        val sy = parts(3).toInt
        val bx = parts(5).toInt
        val by = parts(7).toInt
        val pos = Point(sx, sy)
        val beacon = Point(bx, by)
        val dist = manhattan(pos, beacon)
        Sensor(pos, beacon, dist)
      }.toSeq
    }.get

    println(distress(sensors, 4000000))
  }
}
