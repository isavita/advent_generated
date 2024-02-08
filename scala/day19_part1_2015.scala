import scala.io.Source

object Main extends App {
  val input = Source.fromFile("input.txt").getLines.toList
  val medicine = input.last
  val replacements = input.dropRight(2).map(_.split(" => ")).map(arr => (arr(0), arr(1)))

  val distinctMolecules = replacements.flatMap { case (from, to) =>
    val indexes = medicine.sliding(from.length).zipWithIndex.filter { case (substr, _) => substr == from }.map(_._2)
    indexes.map(index => medicine.patch(index, to, from.length))
  }.distinct

  println(distinctMolecules.length)
}