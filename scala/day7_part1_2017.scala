
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  val programs = input.map { line =>
    val parts = line.split(" -> ")
    val programInfo = parts(0).split(" ")
    val name = programInfo(0)
    val weight = programInfo(1).stripSuffix(")").stripPrefix("(").toInt
    val above = if (parts.length > 1) parts(1).split(", ").toList else List.empty[String]
    (name, weight, above)
  }

  val abovePrograms = programs.flatMap(_._3)
  val bottomProgram = programs.find(p => !abovePrograms.contains(p._1)).get._1

  println(bottomProgram)
}
