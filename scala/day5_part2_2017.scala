
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().map(_.toInt).toArray
  var index = 0
  var steps = 0

  while (index >= 0 && index < input.length) {
    val offset = input(index)
    input(index) += (if (offset >= 3) -1 else 1)
    index += offset
    steps += 1
  }

  println(steps)
}
