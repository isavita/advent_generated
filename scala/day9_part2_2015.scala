import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("input.txt").getLines().toList
  val distances = lines.map { line =>
    val parts = line.split(" = ")
    val cities = parts(0).split(" to ")
    (cities(0), cities(1), parts(1).toInt)
  }

  val cities = distances.flatMap { case (city1, city2, _) => List(city1, city2) }.distinct

  def calculateDistance(route: List[String]): Int = {
    route.sliding(2).map { case List(city1, city2) =>
      distances.find { case (c1, c2, _) => (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1) }.get._3
    }.sum
  }

  val shortestDistance = cities.permutations.map(calculateDistance).min
  val longestDistance = cities.permutations.map(calculateDistance).max

  println(shortestDistance)
  println(longestDistance)
}