import scala.io.Source

object ReindeerOlympics extends App {
  case class Reindeer(name: String, speed: Int, flyTime: Int, restTime: Int)

  val input = Source.fromFile("input.txt").getLines().toList
  val reindeerStats = input.map { line =>
    val split = line.split(" ")
    Reindeer(split(0), split(3).toInt, split(6).toInt, split(13).toInt)
  }

  def distanceTraveled(reindeer: Reindeer, time: Int): Int = {
    val cycleTime = reindeer.flyTime + reindeer.restTime
    val cycles = time / cycleTime
    val remainingTime = time % cycleTime
    val flyingTime = math.min(reindeer.flyTime, remainingTime)
    cycles * reindeer.speed * reindeer.flyTime + flyingTime * reindeer.speed
  }

  val winningDistance = reindeerStats.map(reindeer => distanceTraveled(reindeer, 2503)).max
  println(winningDistance)
}