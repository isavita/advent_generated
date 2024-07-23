
import scala.io.Source
import scala.collection.mutable

object SleighBalancer {

  def main(args: Array[String]): Unit = {
    val weights = readWeights("input.txt")
    val totalWeight = weights.sum
    val targetWeight = totalWeight / 4

    if (totalWeight % 4 != 0) {
      println("Cannot balance the sleigh, total weight is not divisible by 4.")
      return
    }

    val results = findGroups(weights, targetWeight)
    val bestResult = results.minBy { case (count, qe) => (count, qe) }

    println(s"The quantum entanglement of the first group in the ideal configuration is: ${bestResult._2}")
  }

  def readWeights(filename: String): List[Int] = {
    Source.fromFile(filename).getLines().toList.map(_.toInt)
  }

  def findGroups(weights: List[Int], targetWeight: Int): List[(Int, Long)] = {
    val results = mutable.ListBuffer[(Int, Long)]()

    def backtrack(current: List[Int], index: Int, currentWeight: Int, currentCount: Int, currentQE: Long): Unit = {
      if (currentWeight == targetWeight) {
        results += ((currentCount, currentQE))
        return
      }
      if (currentWeight > targetWeight || index >= weights.length) return

      for (i <- index until weights.length) {
        val weight = weights(i)
        backtrack(current :+ weight, i + 1, currentWeight + weight, currentCount + 1, currentQE * weight)
      }
    }

    backtrack(List(), 0, 0, 0, 1)
    results.toList
  }
}
