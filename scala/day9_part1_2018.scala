import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val (players, lastMarble) = readInput("input.txt")
    println(playMarbleGame(players, lastMarble))
  }

  def readInput(filename: String): (Int, Int) = {
    val lines = Source.fromFile(filename).getLines().toList
    val parts = lines.head.split(" ")
    (parts(0).toInt, parts(6).toInt)
  }

  def playMarbleGame(players: Int, lastMarble: Int): Int = {
    var scores = Array.fill(players)(0)
    var current = Marble(0)
    current.next = current
    current.prev = current

    for (marble <- 1 to lastMarble) {
      if (marble % 23 == 0) {
        val player = marble % players
        for (_ <- 0 until 7) {
          current = current.prev
        }
        scores(player) += marble + current.value
        current.prev.next = current.next
        current.next.prev = current.prev
        current = current.next
      } else {
        current = current.next
        val newMarble = Marble(marble, current, current.next)
        current.next.prev = newMarble
        current.next = newMarble
        current = newMarble
      }
    }

    scores.max
  }

  case class Marble(value: Int, var prev: Marble = null, var next: Marble = null)
}