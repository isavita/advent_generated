import scala.io.Source
import scala.util.Try

object Main {
  case class Deck(var cards: List[Int]) {
    def copy(n: Int): Deck = Deck(cards.take(n))
    def score: Int = cards.zipWithIndex.map { case (card, i) => card * (cards.length - i) }.sum
  }

  def playRecursiveCombat(player1: Deck, player2: Deck): (Deck, Deck) = {
    var previousRounds = Set[String]()
    var (p1, p2) = (player1, player2)
    while (p1.cards.nonEmpty && p2.cards.nonEmpty) {
      val roundKey = s"${p1.cards.mkString(",")}|${p2.cards.mkString(",")}"
      if (previousRounds.contains(roundKey)) {
        return (p1, Deck(List()))
      }
      previousRounds += roundKey

      val card1 = p1.cards.head
      val card2 = p2.cards.head
      p1.cards = p1.cards.tail
      p2.cards = p2.cards.tail

      if (p1.cards.length >= card1 && p2.cards.length >= card2) {
        val (subP1, _) = playRecursiveCombat(p1.copy(card1), p2.copy(card2))
        if (subP1.cards.nonEmpty) {
          p1.cards = p1.cards ::: List(card1, card2)
        } else {
          p2.cards = p2.cards ::: List(card2, card1)
        }
      } else {
        if (card1 > card2) {
          p1.cards = p1.cards ::: List(card1, card2)
        } else {
          p2.cards = p2.cards ::: List(card2, card1)
        }
      }
    }
    (p1, p2)
  }

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input.txt")
    val lines = file.getLines().toList
    file.close()

    var player1Deck = Deck(List())
    var player2Deck = Deck(List())
    var currentDeck = player1Deck

    for (line <- lines) {
      if (line.isEmpty) {
        currentDeck = player2Deck
      } else if (line.contains("Player")) {
        // ignore
      } else {
        val card = Try(line.toInt).toOption.get
        currentDeck.cards = currentDeck.cards ::: List(card)
      }
    }

    val (player1, player2) = playRecursiveCombat(player1Deck, player2Deck)
    val winningDeck = if (player1.cards.nonEmpty) player1 else player2
    println(winningDeck.score)
  }
}