
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString
  val result = input.zip(input.tail + input.head).filter(pair => pair._1 == pair._2).map(_._1.asDigit).sum
  println(result)
}
