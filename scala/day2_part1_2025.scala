
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.math.BigInt

object Main {
  case class Range(start: BigInt, end: BigInt)

  def main(args: Array[String]): Unit = {
    // 1. read & parse input
    val raw = new String(Files.readAllBytes(Paths.get("input.txt"))).replaceAll("[\\r\\n]", "").trim
    val ranges = raw.split(",").filter(_.nonEmpty).map { part =>
      val Array(s, e) = part.split("-")
      Range(BigInt(s), BigInt(e))
    }

    // 2. determine maximal k we need to check
    val maxEnd = ranges.map(_.end).max
    val maxK   = (maxEnd.toString.length + 1) / 2   // ceil(len/2)

    // 3. collect unique invalid IDs
    val ids = mutable.HashSet.empty[BigInt]

    for (k <- 1 to maxK) {
      val pow10k   = BigInt(10).pow(k)
      val multiplier = pow10k + 1                     // 10^k + 1
      val minSeed = if (k == 1) BigInt(1) else BigInt(10).pow(k - 1) // 10^(k-1)
      val maxSeed = pow10k - 1                         // 10^k - 1

      ranges.foreach { r =>
        // ceil(start / M)
        val sMin = (r.start + multiplier - 1) / multiplier
        // floor(end / M)
        val sMax = r.end / multiplier

        // intersect with [minSeed, maxSeed]
        val low  = if (sMin > minSeed) sMin else minSeed
        val high = if (sMax < maxSeed) sMax else maxSeed

        var cur = low
        while (cur <= high) {
          ids += cur * multiplier
          cur += 1
        }
      }
    }

    // 4. sum and output
    val total = ids.foldLeft(BigInt(0))(_ + _)
    println(s"Sum of invalid IDs: $total")
  }
}
