
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString
  def decompressedLength(s: String): Long = {
    val marker = "\\((\\d+)x(\\d+)\\)".r
    marker.findFirstMatchIn(s) match {
      case Some(m) =>
        val (a, b) = (m.group(1).toInt, m.group(2).toInt)
        val markerLength = m.end - m.start
        val markerEnd = m.end + a
        a * b + decompressedLength(s.substring(markerEnd))
      case None => s.length
    }
  }
  println(decompressedLength(input))
}
