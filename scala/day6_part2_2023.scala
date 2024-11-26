
import scala.io.Source

object BoatRaces {
  def countWaysToWinRace(time: Long, recordDistance: Long): Long = {
    (0L to time).count { holdTime =>
      val travelTime = time - holdTime
      val distance = holdTime * travelTime
      distance > recordDistance
    }
  }

  def parseInputPart1(lines: List[String]): List[(Long, Long)] = {
    val times = lines.head.split(":\\s+").last.split("\\s+").map(_.toLong)
    val distances = lines.last.split(":\\s+").last.split("\\s+").map(_.toLong)
    times.zip(distances).toList
  }

  def parseInputPart2(lines: List[String]): (Long, Long) = {
    val time = lines.head.split(":\\s+").last.replace(" ", "").toLong
    val distance = lines.last.split(":\\s+").last.replace(" ", "").toLong
    (time, distance)
  }

  def solvePart1(races: List[(Long, Long)]): Long = {
    races.map { case (time, distance) => countWaysToWinRace(time, distance) }
      .product
  }

  def solvePart2(time: Long, distance: Long): Long = {
    countWaysToWinRace(time, distance)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList

    val part1Result = solvePart1(parseInputPart1(lines))
    println(s"Part 1 Result: $part1Result")

    val (time, distance) = parseInputPart2(lines)
    val part2Result = solvePart2(time, distance)
    println(s"Part 2 Result: $part2Result")
  }
}
