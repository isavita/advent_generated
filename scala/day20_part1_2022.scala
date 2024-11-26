
import scala.io.Source

object GrovePositioningSystem {
  def main(args: Array[String]): Unit = {
    // Read input from file
    val input = Source.fromFile("input.txt").getLines().map(_.toLong).toList

    // Create a list of tuples with original index and value
    val numbersWithIndex = input.zipWithIndex

    // Perform mixing
    val mixed = mix(numbersWithIndex)

    // Find grove coordinates
    val coordinates = findGroveCoordinates(mixed)

    // Print result
    println(coordinates.sum)
  }

  def mix(numbers: List[(Long, Int)]): List[(Long, Int)] = {
    numbers.foldLeft(numbers) { (current, number) =>
      val index = current.indexOf(number)
      val removed = current.patch(index, Nil, 1)
      val newIndex = calculateNewIndex(index, number._1, removed.size)
      removed.patch(newIndex, Seq(number), 0)
    }
  }

  def calculateNewIndex(currentIndex: Int, value: Long, listSize: Int): Int = {
    val newIndex = (currentIndex + value) % listSize
    if (newIndex < 0) (newIndex + listSize).toInt else newIndex.toInt
  }

  def findGroveCoordinates(mixed: List[(Long, Int)]): List[Long] = {
    val zeroIndex = mixed.indexWhere(_._1 == 0)
    List(1000, 2000, 3000).map { offset =>
      mixed((zeroIndex + offset) % mixed.size)._1
    }
  }
}
