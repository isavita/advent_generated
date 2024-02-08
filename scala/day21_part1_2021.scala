
import scala.io.Source

object Main extends App {
  val data = Source.fromFile("input.txt").getLines.toList
  val player1Start = data(0).substring(28).trim.toInt
  val player2Start = data(1).substring(28).trim.toInt
  var player1Pos = player1Start
  var player2Pos = player2Start

  var player1Score = 0
  var player2Score = 0

  var dieRoll = 1
  var rollCount = 0

  while (true) {
    // Player 1
    val rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
    rollCount += 3
    dieRoll += 3

    player1Pos = (player1Pos + rolls - 1) % 10 + 1
    player1Score += player1Pos

    if (player1Score >= 1000) {
      println(player2Score * rollCount)
      System.exit(0)
    }

    // Player 2
    val rolls2 = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
    rollCount += 3
    dieRoll += 3

    player2Pos = (player2Pos + rolls2 - 1) % 10 + 1
    player2Score += player2Pos

    if (player2Score >= 1000) {
      println(player1Score * rollCount)
      System.exit(0)
    }
  }
}
