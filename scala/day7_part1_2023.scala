import scala.io.Source
import scala.util.Try

object Main {
  val HighCard = 1
  val OnePair = 2
  val TwoPair = 3
  val ThreeKind = 4
  val FullHouse = 5
  val FourKind = 6
  val FiveKind = 7

  case class Hand(cards: String, bid: Int)
  case class RankedHand(hand: Hand, rank: Int)

  var matches = Array.fill(7)(List[Hand]())

  def findMatches(hands: List[Hand]): Unit = {
    for (hand <- hands) {
      val count = hand.cards.groupBy(identity).mapValues(_.length)

      val value = count.values.product

      value match {
        case 1 => matches(6) = hand :: matches(6)
        case 2 => matches(5) = hand :: matches(5)
        case 3 => matches(3) = hand :: matches(3)
        case 4 => if (count.size == 2) matches(1) = hand :: matches(1) else matches(4) = hand :: matches(4)
        case 5 => matches(0) = hand :: matches(0)
        case 6 => matches(2) = hand :: matches(2)
        case _ => println("oh no")
      }
    }
  }

  def convertAndOrderMatches(): List[RankedHand] = {
    var convertedMatches = List[RankedHand]()

    for (category <- matches) {
      var temp = List[RankedHand]()

      for (hand <- category) {
        val cards = hand.cards.replace("A", "E").replace("T", "A").replace("J", "B").replace("Q", "C").replace("K", "D")
        val num = Try(java.lang.Long.parseLong(cards, 16)).getOrElse(0L)

        temp = RankedHand(hand, num.toInt) :: temp
      }

      temp = temp.sortWith(_.rank > _.rank)

      convertedMatches = convertedMatches ::: temp
    }

    convertedMatches
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val hands = lines.filter(_.nonEmpty).map { line =>
      val cards = """[\dAKQJT]+""".r.findFirstMatchIn(line).get.group(0)
      val bid = """ [\d]+""".r.findFirstMatchIn(line).get.group(0).tail.toInt

      Hand(cards, bid)
    }

    findMatches(hands)

    val convertedMatches = convertAndOrderMatches()

    var total = 0
    for ((hand, i) <- convertedMatches.zipWithIndex) {
      total += hand.hand.bid * (convertedMatches.length - i)
    }

    println(total)
  }
}