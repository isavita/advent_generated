import scala.io.Source

object Main extends App {
  val input = Source.fromFile("input.txt").mkString
  val numbers = raw"\-?\d+".r.findAllIn(input).toList.map(_.toInt)
  val sum = numbers.sum
  println(sum)
}