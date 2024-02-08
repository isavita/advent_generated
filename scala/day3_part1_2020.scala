
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList
  val width = input.head.length
  val height = input.length

  def countTrees(right: Int, down: Int): Int = {
    (0 until height by down).count(i => input(i)((i / down * right) % width) == '#')
  }

  val result = countTrees(3, 1)
  println(result)
}
