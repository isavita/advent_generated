object Main extends App {
  import scala.io.Source

  val filename = "input.txt"
  val source = Source.fromFile(filename)
  val lines = try source.getLines.toList finally source.close()

  val positions = lines.flatMap(_.split(",").map(_.toInt)).sorted

  val minFuel = (positions.head to positions.last).map { i =>
    positions.map(pos => calculateNewFuel(pos, i)).sum
  }.min

  println(minFuel)

  def calculateNewFuel(currentPosition: Int, newPosition: Int): Int = {
    val diff = math.abs(currentPosition - newPosition)
    (diff * (diff + 1)) / 2
  }
}