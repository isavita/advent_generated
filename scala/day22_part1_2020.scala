import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val player1 = lines.drop(1).takeWhile(_.nonEmpty).map(_.toInt)
    val player2 = lines.drop(player1.length + 3).map(_.toInt)

    def playGame(p1: List[Int], p2: List[Int]): (Int, List[Int]) = {
      if (p1.isEmpty) {
        (2, p2)
      } else if (p2.isEmpty) {
        (1, p1)
      } else {
        val card1 = p1.head
        val card2 = p2.head
        if (card1 > card2) {
          playGame(p1.tail ++ List(card1, card2), p2.tail)
        } else {
          playGame(p1.tail, p2.tail ++ List(card2, card1))
        }
      }
    }

    val (_, winningDeck) = playGame(player1, player2)
    val score = winningDeck.reverse.zipWithIndex.map { case (card, index) =>
      card * (index + 1)
    }.sum

    println(score)
  }
}