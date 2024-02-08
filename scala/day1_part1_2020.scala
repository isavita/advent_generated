object Main extends App {
  val expenses = scala.io.Source.fromFile("input.txt").getLines().map(_.toInt).toList
  val result = expenses.combinations(2).find(_.sum == 2020).map(_.product).get
  println(result)
}