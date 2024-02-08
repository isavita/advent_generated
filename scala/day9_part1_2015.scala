import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("input.txt").getLines().toList
  val distances = lines.map(_.split(" = ")).map(arr => (arr(0), arr(1).toInt))
  
  val locations = distances.flatMap { case (str, _) => str.split(" to ") }.distinct
  val perms = locations.permutations.toList
  
  val shortestDistance = perms.map { perm =>
    perm.sliding(2).map { case List(a, b) =>
      distances.find { case (str, _) => str == s"$a to $b" }.getOrElse(distances.find { case (str, _) => str == s"$b to $a" }.get)._2
    }.sum
  }.min
  
  println(shortestDistance)
}