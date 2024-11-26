
import scala.io.Source

object Scratchcards {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    
    // Part 1: Calculate total points
    val totalPoints = input.map(calculateCardPoints).sum
    println(s"Part 1 - Total Points: $totalPoints")
    
    // Part 2: Calculate total scratchcards
    val totalScratchcards = calculateTotalScratchcards(input)
    println(s"Part 2 - Total Scratchcards: $totalScratchcards")
  }
  
  def calculateCardPoints(card: String): Int = {
    val (winningNumbers, myNumbers) = parseCard(card)
    val matchingNumbers = myNumbers.count(winningNumbers.contains)
    
    if (matchingNumbers > 0) math.pow(2, matchingNumbers - 1).toInt else 0
  }
  
  def calculateTotalScratchcards(cards: List[String]): Int = {
    val cardCounts = Array.fill(cards.length)(1)
    
    cards.indices.foreach { i =>
      val (winningNumbers, myNumbers) = parseCard(cards(i))
      val matchingNumbers = myNumbers.count(winningNumbers.contains)
      
      (1 to matchingNumbers).foreach { j =>
        if (i + j < cards.length) {
          cardCounts(i + j) += cardCounts(i)
        }
      }
    }
    
    cardCounts.sum
  }
  
  def parseCard(card: String): (Set[Int], List[Int]) = {
    val parts = card.split(": ")(1).split(" \\| ")
    val winningNumbers = parts(0).split("\\s+").filter(_.nonEmpty).map(_.toInt).toSet
    val myNumbers = parts(1).split("\\s+").filter(_.nonEmpty).map(_.toInt).toList
    
    (winningNumbers, myNumbers)
  }
}
