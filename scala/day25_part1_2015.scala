object Main extends App {
  import scala.io.Source
  import scala.util.matching.Regex

  val source = Source.fromFile("input.txt")
  val input = try source.mkString finally source.close()

  val pattern: Regex = "row (\\d+), column (\\d+)".r
  val Array(row, column) = pattern.findFirstMatchIn(input) match {
    case Some(m) => Array(m.group(1).toInt, m.group(2).toInt)
    case None => throw new IllegalArgumentException("Invalid input format.")
  }

  val pos = getPosition(row, column)
  val code = getCode(pos)

  println(code)

  def getPosition(row: Int, column: Int): Int = (row + column - 2) * (row + column - 1) / 2 + column

  def getCode(position: Int): Long = {
    val startCode = 20151125L
    val multiplier = 252533L
    val modulus = 33554393L

    var code = startCode
    for (_ <- 1 until position) {
      code = (code * multiplier) % modulus
    }
    code.toInt
  }
}