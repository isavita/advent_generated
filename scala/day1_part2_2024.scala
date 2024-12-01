import scala.io.Source

object Day1 extends App {
  val lines = Source.fromFile("input.txt").getLines().filterNot(_.isEmpty).toList
  
  val (leftList, rightList) = lines.map { line =>
    val nums = line.trim.split("\\s+").map(_.toInt)
    (nums.head, nums.last)
  }.unzip
  
  val rightCounts = rightList.groupBy(identity).view.mapValues(_.size).toMap
  
  val similarityScore = leftList.map { num =>
    num.toLong * rightCounts.getOrElse(num, 0)
  }.sum
  
  println(s"Similarity score: $similarityScore")
}
