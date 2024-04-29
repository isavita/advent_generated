import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object ChocolateCharts {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim.toInt
    val scoreboard = ArrayBuffer(3, 7)
    var elf1 = 0
    var elf2 = 1
    var i = 2

    while (i <= input + 10) {
      val sum = scoreboard(elf1) + scoreboard(elf2)
      val digits = sum.toString.map(_.asDigit)
      scoreboard ++= digits
      elf1 = (elf1 + scoreboard(elf1) + 1) % scoreboard.length
      elf2 = (elf2 + scoreboard(elf2) + 1) % scoreboard.length
      i += digits.length
    }

    val result = scoreboard.slice(input, input + 10).mkString
    println(result)
  }
}