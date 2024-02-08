
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val nanobots = input.map(_.split("[^\\d-]+").filter(_.nonEmpty).map(_.toInt))
  val strongest = nanobots.maxBy(_(3))
  val result = nanobots.count(bot => {
    val distance = math.abs(bot(0) - strongest(0)) + math.abs(bot(1) - strongest(1)) + math.abs(bot(2) - strongest(2))
    distance <= strongest(3)
  })
  println(result)
}
