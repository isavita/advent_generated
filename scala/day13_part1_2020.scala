
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val timestamp = input(0).toInt
  val buses = input(1).split(",").filter(_ != "x").map(_.toInt)

  val (busId, waitTime) = buses.map(bus => (bus, bus - (timestamp % bus))).minBy(_._2)
  
  println(busId * waitTime)
}
