import scala.io.Source

object CampCleanup {
  def main(args: Array[String]): Unit = {
    val inputFile = "input.txt"
    val pairs = Source.fromFile(inputFile).getLines().map { line =>
      val Array(range1, range2) = line.split(",")
      val Array(start1, end1) = range1.split("-").map(_.toInt)
      val Array(start2, end2) = range2.split("-").map(_.toInt)
      (start1 to end1, start2 to end2)
    }.toList

    val fullyContainedPairs = pairs.count { case (range1, range2) =>
      (range1.min <= range2.min && range1.max >= range2.max) ||
      (range2.min <= range1.min && range2.max >= range1.max)
    }

    println(s"Number of fully contained pairs: $fullyContainedPairs")
  }
}