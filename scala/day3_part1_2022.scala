
object Day3RucksackReorganization {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines.toList
    val result = input.map { line =>
      val firstCompartment = line.take(line.length / 2).toSet
      val secondCompartment = line.drop(line.length / 2).toSet
      val commonItems = firstCompartment.intersect(secondCompartment)
      commonItems.map(_.toInt - (if (commonItems.head.isLower) 96 else 38)).sum
    }.sum
    println(result)
  }
}
