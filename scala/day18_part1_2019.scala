
import scala.io.Source
import scala.collection.mutable

case class Point(x: Int, y: Int)
case class State(pos: Point, keys: Int)

object Main {
  def main(args: Array[String]): Unit = {
    val grid = Source.fromFile("input.txt").getLines().toArray
    val (start, keyMap) = parseGrid(grid)
    println(findShortestPath(grid, start, keyMap))
  }

  def parseGrid(grid: Array[String]): (Point, Map[Char, Int]) = {
    val keyMap = mutable.Map[Char, Int]()
    var start: Point = null
    var keyCounter = 0

    for (y <- grid.indices) {
      for (x <- grid(y).indices) {
        grid(y)(x) match {
          case '@' => start = Point(x, y)
          case ch if ch.isLower => 
            keyMap(ch) = keyCounter
            keyCounter += 1
          case _ =>
        }
      }
    }
    (start, keyMap.toMap)
  }

  def findShortestPath(grid: Array[String], start: Point, keyMap: Map[Char, Int]): Int = {
    val dirs = Array(Point(0, -1), Point(-1, 0), Point(0, 1), Point(1, 0))
    val visited = mutable.Set[State]()
    val queue = mutable.Queue(State(start, 0))
    var steps = 0

    while (queue.nonEmpty) {
      for (_ <- queue.indices) {
        val current = queue.dequeue()

        if (current.keys == (1 << keyMap.size) - 1) return steps

        for (d <- dirs) {
          val next = Point(current.pos.x + d.x, current.pos.y + d.y)
          if (next.x >= 0 && next.x < grid(0).length && next.y >= 0 && next.y < grid.length) {
            val char = grid(next.y)(next.x)
            if (char != '#' && !(char.isUpper && (current.keys & (1 << keyMap(char.toLower))) == 0)) {
              var newKeys = current.keys
              if (char.isLower) newKeys |= 1 << keyMap(char)
              val newState = State(next, newKeys)
              if (!visited(newState)) {
                visited.add(newState)
                queue.enqueue(newState)
              }
            }
          }
        }
      }
      steps += 1
    }
    -1
  }
}
