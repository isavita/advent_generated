
import scala.io.Source

object Lanternfish {
  def simulateFishPopulation(initialFish: Array[Int], days: Int): Long = {
    // Use an array to track count of fish at each timer value
    val fishCounts = Array.fill(9)(0L)
    
    // Count initial fish
    initialFish.foreach(timer => fishCounts(timer) += 1)
    
    // Simulate days
    for (_ <- 1 to days) {
      val reproducingFish = fishCounts(0)
      
      // Shift all fish counts down
      for (i <- 0 until 8) {
        fishCounts(i) = fishCounts(i + 1)
      }
      
      // Reset reproducing fish to 6
      fishCounts(6) += reproducingFish
      
      // Add new fish at timer 8
      fishCounts(8) = reproducingFish
    }
    
    // Return total fish count
    fishCounts.sum
  }

  def main(args: Array[String]): Unit = {
    // Read input from file
    val input = Source.fromFile("input.txt").getLines().next()
    val initialFish = input.split(",").map(_.toInt)
    
    // Part 1: Simulate 80 days
    val part1Result = simulateFishPopulation(initialFish, 80)
    println(s"Part 1 - Lanternfish after 80 days: $part1Result")
    
    // Part 2: Simulate 256 days
    val part2Result = simulateFishPopulation(initialFish, 256)
    println(s"Part 2 - Lanternfish after 256 days: $part2Result")
  }
}
