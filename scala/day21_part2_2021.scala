
import scala.io.Source

object Main extends App {
  val input = Source.fromFile("input.txt").getLines().mkString("\n").trim
  val result = solve(parseInput(input))
  println(result)

  def parseInput(input: String): Array[Int] = {
    input.split("\n").map { line =>
      line.split(": ")(1).toInt
    }
  }

  def solve(positions: Array[Int]): Long = {
    val (w1, w2) = play(positions, Array(0, 0), 3, true, collection.mutable.Map())
    math.max(w1, w2)
  }

  def play(positions: Array[Int], scores: Array[Int], rollsLeftInTurn: Int, isPlayer1sTurn: Boolean, memo: collection.mutable.Map[String, (Long, Long)]): (Long, Long) = {
    val key = (positions.mkString(",") + scores.mkString(",") + rollsLeftInTurn + isPlayer1sTurn).hashCode.toString
    memo.get(key).getOrElse {
      val playerIndex = if (isPlayer1sTurn) 0 else 1
      val scoresCopy = scores.clone()

      if (rollsLeftInTurn == 0) {
        scoresCopy(playerIndex) += positions(playerIndex)
        if (scoresCopy(playerIndex) >= 21) {
          return if (playerIndex == 0) (1, 0) else (0, 1)
        }
        return play(positions, scoresCopy, 3, !isPlayer1sTurn, memo)
      }

      var wins1 = 0L
      var wins2 = 0L

      for (roll <- 1 to 3) {
        val positionsCopy = positions.clone()
        positionsCopy(playerIndex) = (positionsCopy(playerIndex) + roll - 1) % 10 + 1
        val (r1, r2) = play(positionsCopy, scoresCopy, rollsLeftInTurn - 1, isPlayer1sTurn, memo)
        wins1 += r1
        wins2 += r2
      }

      memo(key) = (wins1, wins2)
      (wins1, wins2)
    }
  }
}
