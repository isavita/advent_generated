
import scala.io.Source
import scala.math.BigInt

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines()
    var total = BigInt(0)
    val k = 12

    for (raw <- lines) {
      val s = raw.trim
      if (s.nonEmpty && s.length >= k) {
        val best = maxSubseq(s, k)
        total += BigInt(best)
      }
    }

    println(s"Total output joltage: $total")
  }

  def maxSubseq(s: String, k: Int): String = {
    val n = s.length
    var toRemove = n - k
    val stack = new Array[Char](n)
    var top = 0

    var i = 0
    while (i < n) {
      val d = s.charAt(i)
      while (toRemove > 0 && top > 0 && stack(top - 1) < d) {
        top -= 1
        toRemove -= 1
      }
      stack(top) = d
      top += 1
      i += 1
    }
    new String(stack, 0, k)
  }
}
