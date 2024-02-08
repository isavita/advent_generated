object Main extends App {
  import scala.io.Source

  def findCombinations(containers: List[Int], target: Int, index: Int, count: Int, minCount: Int, ways: Int): (Int, Int) = {
    if (target == 0) {
      if (minCount == 0 || count < minCount) {
        (count, 1)
      } else if (count == minCount) {
        (count, ways + 1)
      } else {
        (minCount, ways)
      }
    } else if (target < 0 || index >= containers.length) {
      (minCount, ways)
    } else {
      val include = findCombinations(containers, target - containers(index), index + 1, count + 1, minCount, ways)
      val exclude = findCombinations(containers, target, index + 1, count, include._1, include._2)
      (exclude._1, exclude._2)
    }
  }

  val containers = Source.fromFile("input.txt").getLines.map(_.toInt).toList
  val (minCount, ways) = findCombinations(containers, 150, 0, 0, 0, 0)
  println(ways)
}