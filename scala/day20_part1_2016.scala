
import scala.io.Source

case class IpRange(start: Long, end: Long)

object Solution {
  def main(args: Array[String]): Unit = {
    val ipRanges = readIPRanges("input.txt")
    val sortedIpRanges = ipRanges.sortBy(_.start)

    val unblockedIP = findUnblockedIP(sortedIpRanges)
    println(unblockedIP)
  }

  def readIPRanges(filename: String): List[IpRange] = {
    val source = Source.fromFile(filename)
    val ranges = source.getLines().map { line =>
      val parts = line.split("-")
      IpRange(parts(0).toLong, parts(1).toLong)
    }.toList
    source.close()
    ranges
  }

  def findUnblockedIP(ranges: List[IpRange]): Long = {
    var currentIP = 0L
    ranges.foreach { r =>
      if (r.start > currentIP) {
        return currentIP
      }
      if (r.end >= currentIP) {
        currentIP = r.end + 1
      }
    }
    currentIP
  }
}
