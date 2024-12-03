
import scala.io.Source

object RedNosedReports extends App {

  def isSafe(report: String): Boolean = {
    val levels = report.split(" ").map(_.toInt)
    if (levels.length < 2) return true //single level reports are considered safe

    val diffs = levels.zip(levels.tail).map { case (a, b) => b - a }

    val allIncreasing = diffs.forall(_ > 0)
    val allDecreasing = diffs.forall(_ < 0)

    (allIncreasing || allDecreasing) && diffs.forall(diff => math.abs(diff) >= 1 && math.abs(diff) <= 3)
  }


  val input = Source.fromFile("input.txt").getLines().toList

  val safeReportsCount = input.count(isSafe)

  println(s"Number of safe reports: $safeReportsCount")
}
