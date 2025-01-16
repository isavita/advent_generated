
import scala.io.Source
import scala.collection.mutable

object Solution {
  def main(args: Array[String]): Unit = {
    val grid = Source.fromFile("input.txt").getLines().toArray
    val h = grid.length
    val w = grid(0).length
    val antennas = mutable.Map[Char, List[(Int, Int)]]()

    for (y <- 0 until h; x <- 0 until w) {
      val c = grid(y)(x)
      if (c != '.') {
        antennas(c) = antennas.getOrElse(c, List()) :+ (y, x)
      }
    }

    val antinodes = mutable.Set[(Int, Int)]()
    for (coords <- antennas.values) {
      val n = coords.length
      for (i <- 0 until n; j <- i + 1 until n) {
        val (ay, ax) = coords(i)
        val (by, bx) = coords(j)
        val p1y = 2 * ay - by
        val p1x = 2 * ax - bx
        val p2y = 2 * by - ay
        val p2x = 2 * bx - ax

        if (p1y >= 0 && p1y < h && p1x >= 0 && p1x < w) {
          antinodes.add((p1y, p1x))
        }
        if (p2y >= 0 && p2y < h && p2x >= 0 && p2x < w) {
          antinodes.add((p2y, p2x))
        }
      }
    }
    println(antinodes.size)
  }
}
