
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next().toInt

  def factors(n: Int): List[Int] = (1 to math.sqrt(n).toInt).filter(n % _ == 0).flatMap(i => List(i, n / i)).toList

  def sumOfFactors(n: Int): Int = factors(n).map(_ * 10).sum

  def findHouse(input: Int): Int = Stream.from(1).find(sumOfFactors(_) >= input).get

  println(findHouse(input))
}
