import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("input.txt").getLines().toList

  val happinessMap = lines.map { line =>
    val pattern = "(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)\\.".r
    val pattern(person1, gainOrLose, amount, person2) = line
    (person1, person2) -> (if (gainOrLose == "gain") amount.toInt else -amount.toInt)
  }.toMap

  val people = happinessMap.keys.flatMap { case (p1, p2) => List(p1, p2) }.toList.distinct

  def calculateHappiness(arrangement: List[String]): Int = {
    val pairs = arrangement.zip(arrangement.tail :+ arrangement.head)
    pairs.map { case (p1, p2) => happinessMap((p1, p2)) + happinessMap((p2, p1)) }.sum
  }

  val seatingArrangements = people.permutations.map(calculateHappiness).toList
  val result = seatingArrangements.max

  println(result)
}