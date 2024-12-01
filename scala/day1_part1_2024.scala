import scala.io.Source

object Day1 extends App {
  val lines = Source.fromFile("input.txt").getLines().filterNot(_.isEmpty).toList
  
  val (leftList, rightList) = lines.map { line =>
    val nums = line.trim.split("\\s+").map(_.toInt)
    (nums.head, nums.last)
  }.unzip
  
  val totalDistance = leftList.sorted
    .zip(rightList.sorted)
    .map { case (l, r) => (l - r).abs }
    .sum
    
  println(s"Total distance: $totalDistance")
}
