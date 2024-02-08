
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next()
  val positions = input.split(",").map(_.toInt)
  val target = positions.sorted.apply(positions.length / 2)
  val result = positions.map(p => Math.abs(p - target)).sum
  println(result)
}
