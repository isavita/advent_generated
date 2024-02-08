
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  def binaryPartition(s: String, lowerChar: Char, upperChar: Char, range: Range): Int = {
    s.foldLeft(range)((acc, c) => {
      val mid = acc.start + (acc.end - acc.start) / 2
      if (c == lowerChar) Range(acc.start, mid)
      else Range(mid + 1, acc.end)
    }).start
  }

  val seatIds = input.map { line =>
    val row = binaryPartition(line.take(7), 'F', 'B', 0 to 127)
    val col = binaryPartition(line.drop(7), 'L', 'R', 0 to 7)
    row * 8 + col
  }

  println(seatIds.max)
}
