
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next().toInt
  var buffer = List(0)
  var currentPosition = 0

  for (i <- 1 to 2017) {
    currentPosition = (currentPosition + input) % buffer.length + 1
    buffer = buffer.take(currentPosition) ::: List(i) ::: buffer.drop(currentPosition)
  }

  println(buffer((currentPosition + 1) % buffer.length))
}
