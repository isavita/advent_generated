
import scala.io.Source

object CupGame {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().next()
    val cups = Array.fill(input.length + 1)(0)
    var currentCup = 0

    for (i <- input.indices) {
      val cup = input(i).asDigit
      if (i == 0) currentCup = cup
      cups(cup) = input((i + 1) % input.length).asDigit
    }

    for (_ <- 0 until 100) {
      val pickup1 = cups(currentCup)
      val pickup2 = cups(pickup1)
      val pickup3 = cups(pickup2)

      cups(currentCup) = cups(pickup3)

      var destinationCup = if (currentCup == 1) input.length else currentCup - 1
      while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
        destinationCup = if (destinationCup == 1) input.length else destinationCup - 1
      }

      cups(pickup3) = cups(destinationCup)
      cups(destinationCup) = pickup1

      currentCup = cups(currentCup)
    }

    val result = new StringBuilder
    var cup = cups(1)
    while (cup != 1) {
      result.append(cup)
      cup = cups(cup)
    }
    println(result)
  }
}
