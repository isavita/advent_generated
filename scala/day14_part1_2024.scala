
import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val width = 101
    val height = 103
    val robots = Source.fromFile("input.txt").getLines().map { line =>
      val parts = line.split(" ")
      val pPart = parts(0).substring(2)
      val vPart = parts(1).substring(2)
      val pos = pPart.split(",").map(_.toInt)
      val vel = vPart.split(",").map(_.toInt)
      (pos(0), pos(1), vel(0), vel(1))
    }.toArray

    for (_ <- 0 until 100) {
      for (i <- robots.indices) {
        val (px, py, vx, vy) = robots(i)
        var x = (px + vx) % width
        var y = (py + vy) % height
        if (x < 0) x += width
        if (y < 0) y += height
        robots(i) = (x, y, vx, vy)
      }
    }

    var q1 = 0
    var q2 = 0
    var q3 = 0
    var q4 = 0
    for ((x, y, _, _) <- robots) {
      if (x == 50 || y == 51) {}
      else if (x < 50 && y < 51) q1 += 1
      else if (x > 50 && y < 51) q2 += 1
      else if (x < 50 && y > 51) q3 += 1
      else if (x > 50 && y > 51) q4 += 1
    }

    println(q1.toLong * q2 * q3 * q4)
  }
}
