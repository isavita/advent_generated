import scala.io.Source
import scala.util.Try

case class Hailstone(x: Long, y: Long, z: Long, vx: Long, vy: Long, vz: Long)

object HailstoneCollision {
  def main(args: Array[String]): Unit = {
    val hailstones = parseInput("input.txt")
    val testAreaMin = 200000000000000L
    val testAreaMax = 400000000000000L
    val intersections = hailstones.combinations(2).flatMap {
      case Array(h1, h2) =>
        val intersection = calculateIntersection(h1, h2)
        if (intersection.isDefined && isWithinTestArea(intersection.get, testAreaMin, testAreaMax)) {
          Some(1)
        } else {
          None
        }
    }
    println(intersections.sum)
  }

  def parseInput(filename: String): Array[Hailstone] = {
    val lines = Source.fromFile(filename).getLines().toArray
    lines.map { line =>
      val Array(pos, vel) = line.split(" @ ")
      val Array(px, py, pz) = pos.split(", ").map(_.toLong)
      val Array(vx, vy, vz) = vel.split(", ").map(_.toLong)
      Hailstone(px, py, pz, vx, vy, vz)
    }
  }

  def calculateIntersection(h1: Hailstone, h2: Hailstone): Option[(Long, Long)] = {
    val denominator = h1.vx * h2.vy - h1.vy * h2.vx
    if (denominator == 0) {
      None
    } else {
      val t = (h2.x - h1.x) * h2.vy - (h2.y - h1.y) * h2.vx
      val u = (h2.x - h1.x) * h1.vy - (h2.y - h1.y) * h1.vx
      val tValue = t.toDouble / denominator
      val uValue = u.toDouble / denominator
      if (tValue >= 0 && uValue >= 0) {
        Some((h1.x + tValue * h1.vx).toLong, (h1.y + tValue * h1.vy).toLong)
      } else {
        None
      }
    }
  }

  def isWithinTestArea(point: (Long, Long), min: Long, max: Long): Boolean = {
    point._1 >= min && point._1 <= max && point._2 >= min && point._2 <= max
  }
}