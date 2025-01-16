
import scala.io.Source
import scala.collection.mutable

object Solution {
  val mod = 1 << 24
  val numSteps = 2000

  def nextSecret(s: Long): Long = {
    var x = s * 64
    var result = s ^ x
    result &= mod - 1
    x = result / 32
    result ^= x
    result &= mod - 1
    x = result * 2048
    result ^= x
    result &= mod - 1
    result
  }

  def encodeChange4(c1: Int, c2: Int, c3: Int, c4: Int): Int = {
    (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19
  }

  def main(args: Array[String]): Unit = {
    val initials = Source.fromFile("input.txt").getLines().filter(_.nonEmpty).map(_.toLong).toArray

    val buyers = initials.map { initVal =>
      val prices = Array.ofDim[Int](numSteps + 1)
      var s = initVal
      for (j <- 0 to numSteps) {
        prices(j) = (s % 10).toInt
        if (j < numSteps) s = nextSecret(s)
      }
      val changes = Array.ofDim[Int](numSteps)
      for (j <- 0 until numSteps) changes(j) = prices(j + 1) - prices(j)
      (prices, changes)
    }

    val patternCount = 19 * 19 * 19 * 19
    val globalSum = Array.ofDim[Long](patternCount)

    buyers.foreach { case (prices, changes) =>
      val localPrice = Array.fill(patternCount)(-1)
      for (i <- 0 until numSteps - 3) {
        val c1 = changes(i)
        val c2 = changes(i + 1)
        val c3 = changes(i + 2)
        val c4 = changes(i + 3)
        if (c1 >= -9 && c1 <= 9 && c2 >= -9 && c2 <= 9 && c3 >= -9 && c3 <= 9 && c4 >= -9 && c4 <= 9) {
          val idx = encodeChange4(c1, c2, c3, c4)
          if (localPrice(idx) < 0) localPrice(idx) = prices(i + 4)
        }
      }
      for (idx <- 0 until patternCount) {
        if (localPrice(idx) >= 0) globalSum(idx) += localPrice(idx)
      }
    }

    val ans = globalSum.max
    println(ans)
  }
}
