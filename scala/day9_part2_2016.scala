
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString
  def decompressedLength(s: String): Long = {
    if (!s.contains("(")) s.length
    else {
      val marker = "\\((\\d+)x(\\d+)\\)".r
      val m = marker.findFirstMatchIn(s).get
      val (len, rep) = (m.group(1).toInt, m.group(2).toInt)
      val start = s.indexOf('(')
      val end = s.indexOf(')')
      val prefix = s.substring(0, start)
      val suffix = s.substring(end + 1)
      val repeat = s.substring(end + 1, end + 1 + len)
      val rest = s.substring(end + 1 + len)
      prefix.length + rep * decompressedLength(repeat) + decompressedLength(rest)
    }
  }
  println(decompressedLength(input))
}
