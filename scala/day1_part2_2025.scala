
import scala.io.Source

object Main {
  def floorDiv(a: Int, b: Int): Int = {
    val q = a / b
    if (a < 0 && a % b != 0) q - 1 else q
  }

  def main(args: Array[String]): Unit = {
    val dialSize = 100
    var currentPos = 50
    var totalZeroHits = 0L

    for (rawLine <- Source.fromFile("input.txt").getLines()) {
      val line = rawLine.trim
      if (line.nonEmpty) {
        val direction = line.charAt(0)
        val amount = line.substring(1).toInt
        direction match {
          case 'R' =>
            totalZeroHits += (currentPos + amount) / dialSize
            currentPos = (currentPos + amount) % dialSize
          case 'L' =>
            totalZeroHits += floorDiv(currentPos - 1, dialSize) - floorDiv(currentPos - amount - 1, dialSize)
            currentPos = (currentPos - amount) % dialSize
            if (currentPos < 0) currentPos += dialSize
          case _ => throw new IllegalArgumentException(s"Unknown direction '$direction'")
        }
      }
    }

    println(s"The password is: $totalZeroHits")
  }
}
