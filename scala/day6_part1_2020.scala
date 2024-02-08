
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  val groups = input.mkString("\n").split("\n\n")

  val result = groups.map(group => group.replace("\n", "").toSet.size).sum

  println(result)
}
