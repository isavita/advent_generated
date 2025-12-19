import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().filter(_.nonEmpty).toArray
    if (lines.isEmpty) { println("Total rolls removed: 0"); return }
    val rows = lines.length
    val cols = lines(0).length
    val grid = Array.ofDim[Char](rows, cols)
    for (i <- 0 until rows) grid(i) = lines(i).toCharArray
    var total = 0
    while (true) {
      val toRemove = ArrayBuffer.empty[(Int, Int)]
      for (r <- 0 until rows; c <- 0 until cols if grid(r)(c) == '@') {
        var cnt = 0
        var dr = -1
        while (dr <= 1) {
          var dc = -1
          while (dc <= 1) {
            if (dr != 0 || dc != 0) {
              val nr = r + dr
              val nc = c + dc
              if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid(nr)(nc) == '@') cnt += 1
            }
            dc += 1
          }
          dr += 1
        }
        if (cnt < 4) toRemove += ((r, c))
      }
      if (toRemove.isEmpty) { println(s"Total rolls removed: $total"); return }
      total += toRemove.length
      toRemove.foreach { case (r, c) => grid(r)(c) = '.' }
    }
  }
}