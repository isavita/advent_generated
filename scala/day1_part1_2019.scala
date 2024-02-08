
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val totalFuel = input.map(mass => (mass.toInt / 3) - 2).sum
  println(totalFuel)
}
