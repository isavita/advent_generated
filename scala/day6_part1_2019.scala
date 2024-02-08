object Solution extends App {
  import scala.io.Source

  def countOrbits(orbitMap: Map[String, List[String]], start: String, depth: Int): Int = {
    val orbits = orbitMap.getOrElse(start, Nil)
    if (orbits.isEmpty) depth
    else depth + orbits.map(o => countOrbits(orbitMap, o, depth + 1)).sum
  }

  val data = Source.fromFile("input.txt").getLines().toList
  val orbitMap = data.map(_.split("\\)")).groupBy(_(0)).map { case (k, v) => k -> v.map(_(1)).toList }

  val totalOrbits = countOrbits(orbitMap, "COM", 0)
  println(totalOrbits)
}