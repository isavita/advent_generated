
import scala.io.Source

object BridgeRepair {

  def solveEquation(target: Long, numbers: List[Long]): Boolean = {
    def calculate(index: Int, currentResult: Long): Boolean = {
      if (index == numbers.length) {
        currentResult == target
      } else {
        calculate(index + 1, currentResult + numbers(index)) ||
          calculate(index + 1, currentResult * numbers(index))
      }
    }

    if (numbers.isEmpty) {
      target == 0 
    } else {
      calculate(1, numbers.head)
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines = Source.fromFile(filename).getLines().toList

    val validEquations = lines.filter { line =>
      val parts = line.split(": ")
      val target = parts(0).toLong
      val numbers = parts(1).split(" ").map(_.toLong).toList
      solveEquation(target, numbers)
    }

    val calibrationResult = validEquations.map(_.split(": ")(0).toLong).sum

    println(calibrationResult)
  }
}
