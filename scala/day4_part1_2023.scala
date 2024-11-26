
import scala.io.Source

object Scratchcards {
  def calculateCardPoints(card: String): Int = {
    val parts = card.split(":")
    val numberLists = parts(1).split("\\|")
    
    val winningNumbers = numberLists(0).trim.split("\\s+").map(_.toInt).toSet
    val myNumbers = numberLists(1).trim.split("\\s+").map(_.toInt).toSet
    
    val matchingNumbers = myNumbers.intersect(winningNumbers)
    
    if (matchingNumbers.isEmpty) 0
    else math.pow(2, matchingNumbers.size - 1).toInt
  }

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    
    try {
      val totalPoints = Source.fromFile(filename)
        .getLines()
        .map(calculateCardPoints)
        .sum
      
      println(s"Total points: $totalPoints")
    } catch {
      case e: Exception => 
        println(s"Error reading file: ${e.getMessage}")
    }
  }
}
