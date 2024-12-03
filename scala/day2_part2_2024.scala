
import scala.io.Source

object RedNosedReports {

  def isSafe(report: List[Int]): Boolean = {
    if (report.length < 2) return true //Single-level reports are considered safe

    val diffs = report.zip(report.tail).map { case (a, b) => b - a }
    val allPositive = diffs.forall(_ > 0)
    val allNegative = diffs.forall(_ < 0)

    (allPositive || allNegative) && diffs.forall(diff => math.abs(diff) >= 1 && math.abs(diff) <= 3)
  }

  def isSafeWithDampener(report: List[Int]): Boolean = {
    if (isSafe(report)) return true
    
    for (i <- report.indices) {
      val modifiedReport = report.patch(i, Nil, 1)
      if (isSafe(modifiedReport)) return true
    }
    false
  }


  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList

    val reports = input.map(_.split(" ").map(_.toInt).toList)

    val safeReportsPart1 = reports.count(isSafe)
    println(s"Part 1: Number of safe reports: $safeReportsPart1")

    val safeReportsPart2 = reports.count(isSafeWithDampener)
    println(s"Part 2: Number of safe reports with dampener: $safeReportsPart2")
  }
}
