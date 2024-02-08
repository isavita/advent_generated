import scala.io.Source

object Main extends App {
  val filename = "input.txt"
  val file = Source.fromFile(filename)
  var totalDiff = 0

  for (line <- file.getLines) {
    val originalLength = line.length
    val encodedLength = calculateEncodedLength(line)
    totalDiff += encodedLength - originalLength
  }

  file.close()
  println(totalDiff)

  def calculateEncodedLength(s: String): Int = {
    var encoded = "\""
    for (ch <- s) {
      if (ch == '\\' || ch == '"') {
        encoded += "\\"
      }
      encoded += ch
    }
    encoded += "\""
    encoded.length
  }
}