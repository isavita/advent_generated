
import scala.io.Source
import scala.collection.mutable

case class Point(x: Int, y: Int)
case class Sensor(pos: Point, beacon: Point, dist: Int)

object Main {
  def main(args: Array[String]): Unit = {
    val sensors = readSensors("input.txt")
    println(impossible(sensors, 2000000))
  }

  def readSensors(filename: String): Seq[Sensor] = {
    val pattern = "Sensor at x=(\\d+), y=(\\d+): closest beacon is at x=(\\d+), y=(\\d+)".r
    Source.fromFile(filename).getLines().collect {
      case pattern(x, y, bx, by) =>
        val pos = Point(x.toInt, y.toInt)
        val beacon = Point(bx.toInt, by.toInt)
        Sensor(pos, beacon, manhattan(pos, beacon))
    }.toSeq
  }

  def impossible(sensors: Seq[Sensor], y: Int): Int = {
    val pts = mutable.Set[Int]()
    sensors.foreach { s =>
      val dist = s.dist - math.abs(s.pos.y - y)
      for (x <- 0 to dist) {
        pts.add(s.pos.x + x)
        pts.add(s.pos.x - x)
      }
    }
    sensors.foreach { s =>
      if (s.beacon.y == y) pts.remove(s.beacon.x)
    }
    pts.size
  }

  def manhattan(p: Point, q: Point): Int = math.abs(p.x - q.x) + math.abs(p.y - q.y)
}
