
object Main extends App {
  val numbers = scala.io.Source.fromFile("input.txt").getLines().map(_.toLong).toList
  val preambleLength = 25

  def isValidNumber(num: Long, preamble: List[Long]): Boolean = {
    preamble.combinations(2).exists(pair => pair.sum == num)
  }

  val invalidNumber = numbers.sliding(preambleLength + 1).find { case preamble :+ num => !isValidNumber(num, preamble) }.get.last

  println(invalidNumber)
}
