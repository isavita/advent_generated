
import scala.io.Source
import scala.util.Using

object Solution {
  def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)

  def main(args: Array[String]): Unit = {
    val grid = Using(Source.fromFile("input.txt")) { source =>
      source.getLines().toArray
    }.get

    val h = grid.length
    val w = if (h > 0) grid(0).length else 0

    val antennas = grid.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect {
        case (c, x) if c != '.' => (c, (y, x))
      }
    }.groupBy(_._1).map { case (c, list) =>
      (c, list.map(_._2).toArray)
    }

    val linesPerFreq = antennas.map { case (f, coords) =>
      val lines = collection.mutable.Set[String]()
      val n = coords.length
      for (i <- 0 until n; j <- i + 1 until n) {
        val (ay, ax) = coords(i)
        val (by, bx) = coords(j)
        val dy = by - ay
        val dx = bx - ax
        val g = gcd(dy, dx)
        var sy = dy / g
        var sx = dx / g
        if (sx < 0 || (sx == 0 && sy < 0)) {
          sx = -sx
          sy = -sy
        }
        val c = sy * ax - sx * ay
        lines.add(s"$sx,$sy,$c")
      }
      (f, lines.toSet)
    }

    val antinodes = collection.mutable.Set[(Int, Int)]()
    linesPerFreq.values.foreach { lines =>
      lines.foreach { key =>
        val parts = key.split(",").map(_.toInt)
        val sx = parts(0)
        val sy = parts(1)
        val c = parts(2)

        if (sx == 0 && sy == 0) {
          
        } else if (sy == 0) {
          if (c % sx == 0) {
            val y = -c / sx
            if (y >= 0 && y < h) {
              for (x <- 0 until w) {
                antinodes.add((y, x))
              }
            }
          }
        } else if (sx == 0) {
          if (c % sy == 0) {
            val x = c / sy
            if (x >= 0 && x < w) {
              for (y <- 0 until h) {
                antinodes.add((y, x))
              }
            }
          }
        } else {
          for (y <- 0 until h) {
            val val_ = c + sx * y
            if (val_ % sy == 0) {
              val x = val_ / sy
              if (x >= 0 && x < w) {
                antinodes.add((y, x))
              }
            }
          }
        }
      }
    }
    println(antinodes.size)
  }
}
