
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().mkString
  val result = input.sliding(4).indexWhere(_.distinct.length == 4) + 4
  println(result)
}
