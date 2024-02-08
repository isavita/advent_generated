import scala.io.Source
import scala.util.Try

object solution extends App {

  def calculateWaysToWin(time: Int, record: Int): Int = {
    var waysToWin = 0
    for (holdTime <- 1 until time) {
      val travelTime = time - holdTime
      val distance = holdTime * travelTime
      if (distance > record) {
        waysToWin += 1
      }
    }
    waysToWin
  }

  // Read input from file
  var times: List[Int] = Nil
  var distances: List[Int] = Nil

  val source = Source.fromFile("input.txt")
  val lines = source.getLines.toList
  source.close()

  for (line <- lines) {
    val values = line.split(" ").flatMap(value => Try(value.toInt).toOption)
    if (times.isEmpty) {
      times = values.toList
    } else {
      distances = values.toList
    }
  }

  // Calculate total ways to win
  var totalWays = 1
  for (i <- times.indices) {
    val ways = calculateWaysToWin(times(i), distances(i))
    totalWays *= ways
  }

  println(totalWays)
}