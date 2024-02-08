import scala.io.Source
import scala.util.Try

object Main extends App {
  val file = Source.fromFile("input.txt")
  var sum = 0

  for (line <- file.getLines) {
    if (line.nonEmpty) {
      var firstDigit = -1
      var lastDigit = -1

      for (c <- line) {
        if (Character.isDigit(c)) {
          if (firstDigit == -1) {
            firstDigit = Character.getNumericValue(c)
          }
          lastDigit = Character.getNumericValue(c)
        }
      }

      if (firstDigit != -1 && lastDigit != -1) {
        val value = Try(s"${firstDigit}${lastDigit}".toInt).getOrElse(0)
        sum += value
      }
    }
  }

  file.close()
  println(sum)
}