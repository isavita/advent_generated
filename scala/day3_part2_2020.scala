
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList
  val width = input.head.length

  def countTrees(right: Int, down: Int): Long = {
    input.zipWithIndex.count {
      case (line, index) => index % down == 0 && line.charAt((index / down * right) % width) == '#'
    }
  }

  val result = countTrees(1, 1) * countTrees(3, 1) * countTrees(5, 1) * countTrees(7, 1) * countTrees(1, 2)
  println(result)
}
