import java.nio.file.{Files, Paths}
import scala.collection.mutable.{HashSet, ArrayBuffer}
import scala.math.BigInt

object Main extends App {
  val content = new String(Files.readAllBytes(Paths.get("input.txt"))).replaceAll("[\\r\\n]", "").trim
  val parts = content.split(",").filter(_.nonEmpty)
  case class Range(start: BigInt, end: BigInt)
  val ranges = parts.map { p =>
    val b = p.split("-")
    Range(BigInt(b(0)), BigInt(b(1)))
  }

  val found = HashSet[BigInt]()

  for (r <- ranges) {
    val sLen = r.start.toString.length
    val eLen = r.end.toString.length
    for (totalLen <- sLen to eLen) {
      var k = 1
      while (k <= totalLen / 2) {
        if (totalLen % k == 0) {
          val reps = totalLen / k
          var M = BigInt(0)
          var i = 0
          while (i < reps) {
            M += BigInt(10).pow(i * k)
            i += 1
          }
          val minSeed = BigInt(10).pow(k - 1)
          val maxSeed = BigInt(10).pow(k) - 1
          val targetMin = (r.start + M - 1) / M
          val targetMax = r.end / M
          var start = if (targetMin > minSeed) targetMin else minSeed
          var end = if (targetMax < maxSeed) targetMax else maxSeed
          if (start <= end) {
            var cur = start
            while (cur <= end) {
              found += cur * M
              cur += 1
            }
          }
        }
        k += 1
      }
    }
  }

  val totalSum = found.foldLeft(BigInt(0))(_ + _)
  println(s"Sum of invalid IDs: $totalSum")
}