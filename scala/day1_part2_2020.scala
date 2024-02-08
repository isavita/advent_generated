object Day1 extends App {
  val expenses = scala.io.Source.fromFile("input.txt").getLines().toList.map(_.toInt)
  
  val part1 = expenses.combinations(2).find(_.sum == 2020).map(_.product).getOrElse(0)
  val part2 = expenses.combinations(3).find(_.sum == 2020).map(_.product).getOrElse(0)
  
  println(part1)
  println(part2)
}