
object Main extends App {
  val containers = scala.io.Source.fromFile("input.txt").getLines().map(_.toInt).toList
  def countCombinations(target: Int, containers: List[Int]): Int = {
    if (target == 0) 1
    else if (target < 0 || containers.isEmpty) 0
    else countCombinations(target - containers.head, containers.tail) + countCombinations(target, containers.tail)
  }
  println(countCombinations(150, containers))
}
