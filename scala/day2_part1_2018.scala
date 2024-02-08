
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val (twos, threes) = input.map(id => id.groupBy(identity).mapValues(_.length).values.toSet).foldLeft((0, 0)) {
    case ((twos, threes), counts) => (twos + counts.count(_ == 2), threes + counts.count(_ == 3))
  }
  println(twos * threes)
}
