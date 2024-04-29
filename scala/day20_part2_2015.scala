import scala.io.Source
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim.toInt / 11
    val houses = new Array[Int](input + 1)

    for (elf <- 1 to input) {
      for (house <- elf to math.min(elf * 50, input) by elf) {
        houses(house) += elf
      }
    }

    val houseNumber = houses.indexOf(houses.find(_ >= input).get)
    println(houseNumber)
  }
}