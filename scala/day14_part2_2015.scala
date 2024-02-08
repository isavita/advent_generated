
object Main extends App {
  case class Reindeer(name: String, speed: Int, flyTime: Int, restTime: Int)

  val reindeerList = io.Source.fromFile("input.txt").getLines().toList.map { line =>
    val split = line.split(" ")
    Reindeer(split(0), split(3).toInt, split(6).toInt, split(13).toInt)
  }

  def distanceAfterTime(reindeer: Reindeer, time: Int): Int = {
    val cycleTime = reindeer.flyTime + reindeer.restTime
    val fullCycles = time / cycleTime
    val remainingTime = time % cycleTime
    val flyTime = math.min(reindeer.flyTime, remainingTime)
    (fullCycles * reindeer.flyTime + flyTime) * reindeer.speed
  }

  val part1Answer = reindeerList.map(reindeer => distanceAfterTime(reindeer, 2503)).max
  println(part1Answer)

  var points = Map[String, Int]().withDefaultValue(0)

  for (time <- 1 to 2503) {
    val distances = reindeerList.map(reindeer => distanceAfterTime(reindeer, time))
    val maxDistance = distances.max
    val winners = reindeerList.filter(reindeer => distanceAfterTime(reindeer, time) == maxDistance)
    winners.foreach(winner => points += (winner.name -> (points(winner.name) + 1)))
  }

  val part2Answer = points.values.max
  println(part2Answer)
}
