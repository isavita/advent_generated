
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().map(_.toInt).toArray
  var index = 0
  var steps = 0

  while (index >= 0 && index < input.length) {
    val jump = input(index)
    input(index) += 1
    index += jump
    steps += 1
  }

  println(steps)
}
