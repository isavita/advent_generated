
object Stones {
  def evenDigits(s: String): Boolean = s.length % 2 == 0

  def trimLeadingZeros(s: String): String = {
    val trimmed = s.dropWhile(_ == '0')
    if (trimmed.isEmpty) "0" else trimmed
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("input.txt")
    var stones = try source.getLines.next().split("\\s+").toList finally source.close()

    for (_ <- 0 until 25) {
      val nextStones = stones.flatMap { s =>
        if (s == "0") List("1")
        else if (evenDigits(s)) {
          val mid = s.length / 2
          List(trimLeadingZeros(s.substring(0, mid)), trimLeadingZeros(s.substring(mid)))
        } else List((s.toLong * 2024).toString)
      }
      stones = nextStones
    }
    println(stones.length)
  }
}
