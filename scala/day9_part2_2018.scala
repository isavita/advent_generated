
import scala.collection.mutable

object MarbleMania {

  case class Node(value: Int, var prev: Node, var next: Node)

  def play(players: Int, lastMarble: Int): Long = {
    val scores = mutable.Map.empty[Int, Long].withDefaultValue(0L)
    var current = Node(0, null, null)
    current.prev = current
    current.next = current

    for (marble <- 1 to lastMarble) {
      if (marble % 23 == 0) {
        val player = (marble - 1) % players + 1
        val toRemove = (1 to 7).foldLeft(current)((node, _) => node.prev)
        scores(player) += marble + toRemove.value
        current = toRemove.next
        toRemove.prev.next = toRemove.next
        toRemove.next.prev = toRemove.prev
      } else {
        val insertAfter = current.next
        val newNode = Node(marble, insertAfter, insertAfter.next)
        insertAfter.next.prev = newNode
        insertAfter.next = newNode
        current = newNode
      }
    }

    scores.values.max
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().next()
    val Array(playersStr, lastMarbleStr) = input.split(" players; last marble is worth | points").filter(_.nonEmpty)
    val players = playersStr.toInt
    val lastMarble = lastMarbleStr.toInt

    println(s"Part 1: ${play(players, lastMarble)}")
    println(s"Part 2: ${play(players, lastMarble * 100)}")
  }
}
