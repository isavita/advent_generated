
import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("input.txt").getLines().toList
  val adapters = 0 :: lines.map(_.toInt).sorted

  val arrangements = countArrangements(adapters)
  println(arrangements)

  def countArrangements(adapters: List[Int]): Long = {
    var ways = Map(0 -> 1L)

    for (i <- 1 until adapters.length) {
      val currentJoltage = adapters(i)
      ways += currentJoltage -> (1 to 3).map(diff => ways.getOrElse(currentJoltage - diff, 0L)).sum
    }

    ways(adapters.last)
  }
}
