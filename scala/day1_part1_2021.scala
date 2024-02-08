object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList.map(_.toInt)
  val count = input.sliding(2).count { case List(a, b) => b > a }
  println(count)
}