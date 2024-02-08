
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.next.split(",").map(_.toInt).toSeq

  def playGame(startingNumbers: Seq[Int], turns: Int): Int = {
    var memory = Map[Int, Int]()
    var lastNumber = 0
    for (i <- 0 until startingNumbers.length - 1) {
      memory += (startingNumbers(i) -> (i + 1))
    }
    lastNumber = startingNumbers.last
    for (i <- startingNumbers.length until turns) {
      val nextNumber = if (memory.contains(lastNumber)) i - memory(lastNumber) else 0
      memory += (lastNumber -> i)
      lastNumber = nextNumber
    }
    lastNumber
  }

  println(playGame(input, 2020))
}
