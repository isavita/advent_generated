
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val totalPaper = input.map { line =>
    val Array(l, w, h) = line.split("x").map(_.toInt)
    val side1 = l * w
    val side2 = w * h
    val side3 = h * l
    val extra = List(side1, side2, side3).min
    2 * side1 + 2 * side2 + 2 * side3 + extra
  }.sum
  println(totalPaper)
}
