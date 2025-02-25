
object Solution {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile("input.txt").getLines().toList
    val patterns = lines.head.split(",").map(_.trim)
    val designs = lines.drop(2)
    val totalWays = designs.map(countWays(_, patterns)).sum
    println(totalWays)
  }

  def countWays(design: String, patterns: Array[String]): Long = {
    val n = design.length
    val dp = Array.ofDim[Long](n + 1)
    dp(0) = 1
    for (i <- 1 to n) {
      for (p <- patterns) {
        val lp = p.length
        if (i >= lp && design.substring(i - lp, i) == p) {
          dp(i) += dp(i - lp)
        }
      }
    }
    dp(n)
  }
}
