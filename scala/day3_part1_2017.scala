object Day3 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next().toInt

  def calculateSteps(square: Int): Int = {
    val root = math.ceil(math.sqrt(square)).toInt
    val side = if (root % 2 == 0) root + 1 else root
    val stepsToCenter = (side - 1) / 2
    val stepsOnSide = (square - (side - 2) * (side - 2)) % (side - 1)
    val stepsToMiddleOfSide = math.abs(stepsOnSide - stepsToCenter)
    stepsToCenter + stepsToMiddleOfSide
  }

  println(calculateSteps(input))
}