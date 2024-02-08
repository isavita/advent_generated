
object Main extends App {
  val lines = scala.io.Source.fromFile("input.txt").getLines.toList
  val message1 = lines.transpose.map(_.groupBy(identity).maxBy(_._2.size)._1).mkString
  val message2 = lines.transpose.map(_.groupBy(identity).minBy(_._2.size)._1).mkString
  println(message1)
  println(message2)
}
