
import scala.io.Source
import scala.collection.mutable

object SpaceStoichiometry {
  def parseReactions(input: List[String]): Map[String, (Int, List[(String, Int)])] = {
    input.map { line =>
      val parts = line.split(" => ")
      val inputs = parts(0).split(", ").map { input =>
        val Array(qty, chemical) = input.split(" ")
        (chemical, qty.toInt)
      }.toList
      val Array(outputQty, outputChemical) = parts(1).split(" ")
      (outputChemical, (outputQty.toInt, inputs))
    }.toMap
  }

  def calculateOreForFuel(reactions: Map[String, (Int, List[(String, Int)])], fuelAmount: Long): Long = {
    def calculateOre(target: String, amount: Long, surplus: mutable.Map[String, Long]): Long = {
      if (target == "ORE") return amount

      val (outputQty, inputs) = reactions(target)
      val multiplier = math.ceil(amount.toDouble / outputQty).toLong
      val extraProduced = multiplier * outputQty - amount

      surplus(target) = surplus.getOrElse(target, 0L) + extraProduced

      inputs.foldLeft(0L) { (oreUsed, input) =>
        val (chemical, requiredQty) = input
        val needed = multiplier * requiredQty
        val surplusAvailable = surplus.getOrElse(chemical, 0L)
        val actualNeeded = math.max(0, needed - surplusAvailable)

        surplus(chemical) = math.max(0, surplusAvailable - needed)
        oreUsed + calculateOre(chemical, actualNeeded, surplus)
      }
    }

    calculateOre("FUEL", fuelAmount, mutable.Map.empty[String, Long])
  }

  def binarySearchFuel(reactions: Map[String, (Int, List[(String, Int)])], totalOre: Long): Long = {
    var low = 0L
    var high = totalOre

    while (low < high) {
      val mid = (low + high + 1) / 2
      val oreNeeded = calculateOreForFuel(reactions, mid)

      if (oreNeeded <= totalOre) {
        low = mid
      } else {
        high = mid - 1
      }
    }

    low
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val reactions = parseReactions(input)

    // Part 1
    val oreForOneFuel = calculateOreForFuel(reactions, 1)
    println(s"Part 1: $oreForOneFuel")

    // Part 2
    val totalOre = 1_000_000_000_000L
    val maxFuel = binarySearchFuel(reactions, totalOre)
    println(s"Part 2: $maxFuel")
  }
}
