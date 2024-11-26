
import scala.io.Source

object SubterraneanSustainability {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val initialState = input.head.split(": ")(1)
    val rules = input.drop(2).map { line =>
      val Array(pattern, result) = line.split(" => ")
      (pattern, result)
    }.toMap

    println(s"Part 1: ${calculatePlantSum(initialState, rules, 20)}")
    println(s"Part 2: ${calculateLargePotSum(initialState, rules)}")
  }

  def calculatePlantSum(initialState: String, rules: Map[String, String], generations: Int): Int = {
    var state = initialState
    var leftPadding = 0

    (1 to generations).foreach { _ =>
      state = s".....$state....."
      leftPadding += 5

      val nextState = state.sliding(5).map { window =>
        rules.getOrElse(window, ".")
      }.mkString

      state = nextState
    }

    state.zipWithIndex.filter(_._1 == '#')
      .map { case (_, idx) => idx - leftPadding }
      .sum
  }

  def calculateLargePotSum(initialState: String, rules: Map[String, String]): Long = {
    var state = initialState
    var leftPadding = 0
    var previousSum = 0L
    var previousDiff = 0L
    var generation = 0

    while (generation < 50000000000L) {
      state = s".....$state....."
      leftPadding += 5

      val nextState = state.sliding(5).map { window =>
        rules.getOrElse(window, ".")
      }.mkString

      state = nextState
      generation += 1

      val currentSum = state.zipWithIndex.filter(_._1 == '#')
        .map { case (_, idx) => idx - leftPadding }
        .sum

      val currentDiff = currentSum - previousSum

      if (currentDiff == previousDiff) {
        // Pattern stabilized, calculate final sum
        return currentSum + (50000000000L - generation) * currentDiff
      }

      previousSum = currentSum
      previousDiff = currentDiff
    }

    0L
  }
}
