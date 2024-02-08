
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString
  val result = reactPolymer(input)
  println(result.length)

  def reactPolymer(polymer: String): String = {
    polymer.foldLeft("") {
      case (acc, unit) if acc.nonEmpty && unit != acc.last && unit.toLower == acc.last.toLower => acc.init
      case (acc, unit) => acc + unit
    }
  }
}
