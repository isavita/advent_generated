
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.next

  def isTrap(left: Char, center: Char, right: Char): Char = {
    if ((left == '^' && center == '^' && right == '.') ||
        (center == '^' && right == '^' && left == '.') ||
        (left == '^' && center == '.' && right == '.') ||
        (right == '^' && center == '.' && left == '.')) '^'
    else '.'
  }

  var row = input
  var safeCount = row.count(_ == '.')

  for (_ <- 1 until 40) {
    val newRow = (0 until row.length).map { i =>
      val left = if (i == 0) '.' else row(i - 1)
      val center = row(i)
      val right = if (i == row.length - 1) '.' else row(i + 1)
      isTrap(left, center, right)
    }.mkString
    safeCount += newRow.count(_ == '.')
    row = newRow
  }

  println(safeCount)
}
