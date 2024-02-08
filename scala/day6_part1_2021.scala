
object Lanternfish extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next().split(",").map(_.toInt).toList

  def simulateDays(ages: List[Int], days: Int): Int = {
    if (days == 0) ages.length
    else {
      val newAges = ages.map(age => if (age == 0) 6 else age - 1) ++ List.fill(ages.count(_ == 0))(8)
      simulateDays(newAges, days - 1)
    }
  }

  val result = simulateDays(input, 80)
  println(result)
}
