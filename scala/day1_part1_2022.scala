import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    var maxCalories = 0
    var currentCalories = 0

    val file = Source.fromFile(filename)
    for (line <- file.getLines) {
      if (line.isEmpty) {
        if (currentCalories > maxCalories) {
          maxCalories = currentCalories
        }
        currentCalories = 0
      } else {
        currentCalories += line.toInt
      }
    }

    if (currentCalories > maxCalories) {
      maxCalories = currentCalories
    }

    file.close()

    println(maxCalories)
  }
}