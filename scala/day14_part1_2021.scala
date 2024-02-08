
import scala.io.Source

object Main extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val polymer = lines.head
  val rules = lines.tail.filter(_.nonEmpty).map(_.split(" -> ")).map(arr => (arr(0), arr(1))).toMap

  def applyInsertion(polymer: String, rules: Map[String, String]): String = {
    val newPolymer = new StringBuilder
    for (i <- polymer.indices.dropRight(1)) {
      newPolymer.append(polymer(i))
      if (rules.contains(polymer.substring(i, i + 2))) {
        newPolymer.append(rules(polymer.substring(i, i + 2)))
      }
    }
    newPolymer.append(polymer.last)
    newPolymer.toString()
  }

  def countElements(polymer: String): Map[Char, Int] = {
    polymer.groupBy(identity).view.mapValues(_.length).toMap
  }

  val iterations = 10
  var currentPolymer = polymer
  for (_ <- 1 to iterations) {
    currentPolymer = applyInsertion(currentPolymer, rules)
  }

  val counts = countElements(currentPolymer)
  val (min, max) = counts.values.foldLeft(Int.MaxValue -> Int.MinValue) { case ((min, max), count) =>
    (math.min(min, count), math.max(max, count))
  }

  println(max - min)
}
