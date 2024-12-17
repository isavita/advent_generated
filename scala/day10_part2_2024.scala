
import scala.io.Source
import scala.collection.mutable

object Solution {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    val nr = lines.length
    val nc = lines(0).length
    val grid = Array.ofDim[Int](nr, nc)
    for (i <- 0 until nr; j <- 0 until nc) {
      grid(i)(j) = lines(i)(j) - '0'
    }

    val dp = Array.ofDim[Long](nr, nc)
    for (i <- 0 until nr; j <- 0 until nc) {
      dp(i)(j) = -1
    }

    val dirs = Array((1, 0), (-1, 0), (0, 1), (0, -1))

    def dfs(r: Int, c: Int): Long = {
      if (dp(r)(c) != -1) {
        return dp(r)(c)
      }
      val h = grid(r)(c)
      if (h == 9) {
        dp(r)(c) = 1
        return 1
      }
      var sum: Long = 0
      for ((dr, dc) <- dirs) {
        val nr2 = r + dr
        val nc2 = c + dc
        if (nr2 >= 0 && nr2 < nr && nc2 >= 0 && nc2 < nc && grid(nr2)(nc2) == h + 1) {
          sum += dfs(nr2, nc2)
        }
      }
      dp(r)(c) = sum
      sum
    }

    var total: Long = 0
    for (r <- 0 until nr; c <- 0 until nc) {
      if (grid(r)(c) == 0) {
        total += dfs(r, c)
      }
    }
    println(total)
  }
}
