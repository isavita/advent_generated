
import scala.io.Source
import java.io.PrintWriter

object Main extends App {
  val lines = Source.fromFile("input.txt").getLines()
  var pos = 50
  var zeros = 0
  for (raw <- lines) {
    val line = raw.trim
    if (line.nonEmpty) {
      val dir = line.charAt(0)
      val amount = line.substring(1).toInt
      pos = (dir match {
        case 'R' => pos + amount
        case 'L' => pos - amount
      }) % 100
      if (pos < 0) pos += 100
      if (pos == 0) zeros += 1
    }
  }
  println(s"The password is: $zeros")
}
