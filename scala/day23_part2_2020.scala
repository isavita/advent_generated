
import scala.io.Source

object Main {
  val totalCups = 1000000
  val totalMoves = 10000000

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().next()
    val cups = new Array[Int](totalCups + 1)
    var lastCup = 0

    input.zipWithIndex.foreach { case (char, i) =>
      val cup = char.toString.toInt
      if (i > 0) cups(lastCup) = cup
      lastCup = cup
    }

    (input.length + 1 to totalCups).foreach { i =>
      cups(lastCup) = i
      lastCup = i
    }
    cups(lastCup) = input.head.toString.toInt

    var currentCup = input.head.toString.toInt
    (0 until totalMoves).foreach { _ =>
      val pickup1 = cups(currentCup)
      val pickup2 = cups(pickup1)
      val pickup3 = cups(pickup2)

      cups(currentCup) = cups(pickup3)

      var destinationCup = currentCup - 1
      if (destinationCup == 0) destinationCup = totalCups
      while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
        destinationCup -= 1
        if (destinationCup == 0) destinationCup = totalCups
      }

      cups(pickup3) = cups(destinationCup)
      cups(destinationCup) = pickup1

      currentCup = cups(currentCup)
    }

    val cup1 = cups(1)
    val cup2 = cups(cup1)
    println(cup1.toLong * cup2.toLong)
  }
}
