
import scala.io.Source

object FirewallRules {
  def main(args: Array[String]): Unit = {
    // Read the input from the file
    val filename = "input.txt"
    val blockedRanges = Source.fromFile(filename).getLines().toList

    // Parse the blocked ranges into a list of tuples (start, end)
    val ranges = blockedRanges.map { line =>
      val Array(start, end) = line.split("-").map(_.toLong)
      (start, end)
    }.sortBy(_._1)

    // Find the lowest-valued IP that is not blocked
    val lowestAllowedIP = findLowestAllowedIP(ranges)
    println(s"The lowest-valued IP that is not blocked: $lowestAllowedIP")

    // Count the number of allowed IPs
    val allowedCount = countAllowedIPs(ranges)
    println(s"Number of allowed IPs: $allowedCount")
  }

  def findLowestAllowedIP(ranges: List[(Long, Long)]): Long = {
    var lowestIP = 0L

    for ((start, end) <- ranges) {
      if (lowestIP < start) {
        return lowestIP // Found the lowest allowed IP
      }
      // Move the lowestIP pointer to the end of the current blocked range
      lowestIP = Math.max(lowestIP, end + 1)
    }

    lowestIP // If all ranges are checked, return the next possible IP
  }

  def countAllowedIPs(ranges: List[(Long, Long)]): Long = {
    var allowedCount = 0L
    var currentIP = 0L

    for ((start, end) <- ranges) {
      if (currentIP < start) {
        allowedCount += start - currentIP // Count the allowed IPs before the blocked range
      }
      currentIP = Math.max(currentIP, end + 1) // Move currentIP to the end of the blocked range
    }

    // Count the remaining allowed IPs after the last blocked range
    allowedCount += (4294967295L - currentIP + 1)

    allowedCount
  }
}
