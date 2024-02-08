
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList
  val transposed = input.transpose
  val result = transposed.map(_.groupBy(identity).maxBy(_._2.size)._1).mkString
  println(result)
}
