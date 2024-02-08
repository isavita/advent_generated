
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().map(_.toInt).toList.sorted
  val adapters = 0 +: input :+ (input.last + 3)
  val differences = adapters.sliding(2).map { case List(a, b) => b - a }.toList
  val ones = differences.count(_ == 1)
  val threes = differences.count(_ == 3)
  println(ones * threes)
}
