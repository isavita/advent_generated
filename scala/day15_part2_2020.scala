
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.next.split(",").map(_.toInt).toList

  def playGame(startingNumbers: List[Int], targetTurn: Int): Int = {
    var memory = Map[Int, Int]()
    var turn = 1
    var lastNum = 0

    startingNumbers.foreach { num =>
      memory += (num -> turn)
      lastNum = num
      turn += 1
    }

    while (turn <= targetTurn) {
      val nextNum = memory.get(lastNum) match {
        case Some(prevTurn) => turn - prevTurn - 1
        case None => 0
      }
      memory += (lastNum -> (turn - 1))
      lastNum = nextNum
      turn += 1
    }

    lastNum
  }

  println(playGame(input, 2020))
  println(playGame(input, 30000000))
}
