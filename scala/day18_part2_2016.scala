
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next()

  def isTrap(left: Char, center: Char, right: Char): Boolean = {
    (left == '^' && center == '^' && right == '.') ||
      (center == '^' && right == '^' && left == '.') ||
      (left == '^' && center == '.' && right == '.') ||
      (right == '^' && center == '.' && left == '.')
  }

  def countSafeTiles(input: String, numRows: Int): Int = {
    var safeCount = input.count(_ == '.')

    var currentRow = input
    for (_ <- 1 until numRows) {
      var newRow = ""
      for (i <- input.indices) {
        val left = if (i == 0) '.' else currentRow(i - 1)
        val center = currentRow(i)
        val right = if (i == input.length - 1) '.' else currentRow(i + 1)

        if (isTrap(left, center, right)) newRow += '^'
        else newRow += '.'
      }
      safeCount += newRow.count(_ == '.')
      currentRow = newRow
    }

    safeCount
  }

  println(countSafeTiles(input, 40)) // Part One
  println(countSafeTiles(input, 400000)) // Part Two
}
