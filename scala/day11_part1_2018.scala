
object Day11 extends App {
  val serialNumber = scala.io.Source.fromFile("input.txt").getLines().next().toInt

  def powerLevel(x: Int, y: Int): Int = {
    val rackId = x + 10
    val power = rackId * y + serialNumber
    val hundredsDigit = (power * rackId / 100) % 10
    hundredsDigit - 5
  }

  val grid = Array.ofDim[Int](300, 300)
  for (x <- 0 until 300; y <- 0 until 300) {
    grid(x)(y) = powerLevel(x + 1, y + 1)
  }

  def totalPower(x: Int, y: Int): Int = {
    (for (dx <- 0 until 3; dy <- 0 until 3) yield grid(x + dx)(y + dy)).sum
  }

  val maxPower = (for (x <- 0 until 297; y <- 0 until 297) yield (x, y, totalPower(x, y))).maxBy(_._3)
  println(s"${maxPower._1 + 1},${maxPower._2 + 1}")
}
