
object Day22 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  def dealIntoNewStack(deck: List[Int]): List[Int] = deck.reverse

  def cut(deck: List[Int], n: Int): List[Int] = {
    if (n >= 0) {
      deck.drop(n) ++ deck.take(n)
    } else {
      deck.takeRight(-n) ++ deck.dropRight(-n)
    }
  }

  def dealWithIncrement(deck: List[Int], n: Int): List[Int] = {
    val newDeck = Array.fill(deck.length)(-1)
    var pos = 0
    for (i <- deck.indices) {
      newDeck(pos) = deck(i)
      pos = (pos + n) % deck.length
    }
    newDeck.toList
  }

  def shuffleDeck(deckSize: Int, instructions: List[String]): List[Int] = {
    var deck = (0 until deckSize).toList
    for (instr <- instructions) {
      if (instr == "deal into new stack") {
        deck = dealIntoNewStack(deck)
      } else if (instr.startsWith("cut")) {
        val n = instr.split(" ")(1).toInt
        deck = cut(deck, n)
      } else if (instr.startsWith("deal with increment")) {
        val n = instr.split(" ").last.toInt
        deck = dealWithIncrement(deck, n)
      }
    }
    deck
  }

  val deckSize = 10007
  val instructions = input
  val finalDeck = shuffleDeck(deckSize, instructions)
  val result = finalDeck.indexOf(2019)

  println(result)
}
