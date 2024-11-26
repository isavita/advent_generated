
import scala.io.Source

object CamelCards {
  // Define hand types
  enum HandType:
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard

  // Card strength for part 1 and part 2
  def cardStrength(card: Char, useJokers: Boolean): Int = card match
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => if useJokers then 1 else 11
    case 'T' => 10
    case x => x.asDigit

  // Determine hand type
  def getHandType(hand: String, useJokers: Boolean): HandType = {
    val cardCounts = hand.groupBy(identity).mapValues(_.length)
    
    if useJokers then {
      val jokerCount = cardCounts.getOrElse('J', 0)
      val nonJokerCounts = cardCounts.filter(_._1 != 'J').values.toList.sorted.reverse

      val adjustedCounts = if nonJokerCounts.isEmpty then List(jokerCount)
                            else (nonJokerCounts.head + jokerCount) +: nonJokerCounts.tail

      determineHandType(adjustedCounts)
    } else {
      determineHandType(cardCounts.values.toList.sorted.reverse)
    }
  }

  def determineHandType(counts: List[Int]): HandType = counts match
    case 5 :: _ => HandType.FiveOfAKind
    case 4 :: _ => HandType.FourOfAKind
    case 3 :: 2 :: _ => HandType.FullHouse
    case 3 :: _ => HandType.ThreeOfAKind
    case 2 :: 2 :: _ => HandType.TwoPair
    case 2 :: _ => HandType.OnePair
    case _ => HandType.HighCard

  def compareHands(hand1: String, hand2: String, useJokers: Boolean): Boolean = {
    val type1 = getHandType(hand1, useJokers)
    val type2 = getHandType(hand2, useJokers)

    if type1 != type2 then type1.ordinal < type2.ordinal
    else hand1.zip(hand2).find { case (c1, c2) => 
      cardStrength(c1, useJokers) != cardStrength(c2, useJokers)
    }.map { case (c1, c2) => 
      cardStrength(c1, useJokers) > cardStrength(c2, useJokers)
    }.getOrElse(false)
  }

  def calculateTotalWinnings(hands: List[(String, Int)], useJokers: Boolean): Int = {
    val sortedHands = hands.sortWith((a, b) => compareHands(b._1, a._1, useJokers))
    sortedHands.zipWithIndex.map { case ((_, bid), rank) => bid * (rank + 1) }.sum
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val hands = input.map { line =>
      val Array(hand, bid) = line.split(" ")
      (hand, bid.toInt)
    }

    val part1Result = calculateTotalWinnings(hands, useJokers = false)
    val part2Result = calculateTotalWinnings(hands, useJokers = true)

    println(s"Part 1: $part1Result")
    println(s"Part 2: $part2Result")
  }
}
