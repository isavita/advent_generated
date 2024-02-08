
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val buses = input(1).split(",").zipWithIndex.filter(_._1 != "x").map { case (id, index) => (id.toLong, index.toLong) }

  def findEarliestTimestamp(buses: Array[(Long, Long)]): Long = {
    var timestamp = 0L
    var step = 1L

    buses.foreach { case (busId, offset) =>
      while ((timestamp + offset) % busId != 0) {
        timestamp += step
      }
      step *= busId
    }

    timestamp
  }

  println(findEarliestTimestamp(buses))
}
