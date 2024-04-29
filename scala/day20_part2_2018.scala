import scala.io.Source
import scala.collection.mutable

object Main {
  type Point = (Int, Int)
  type DoorMap = mutable.HashMap[Point, mutable.HashSet[Point]]

  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("input.txt").mkString
    val regex = data.substring(1, data.length - 1)
    val dm = buildMap(regex)
    val rooms = countRooms(dm, 1000)
    println(rooms)
  }

  def buildMap(regex: String): DoorMap = {
    val dm = new mutable.HashMap[Point, mutable.HashSet[Point]]
    var stack = new mutable.Stack[Point]
    var cp = (0, 0)
    for (c <- regex) {
      c match {
        case '(' => stack.push(cp)
        case '|' => cp = stack.head
        case ')' => {
          cp = stack.head
          stack.pop()
        }
        case _ => {
          val np = move(cp, c)
          if (!dm.contains(cp)) {
            dm(cp) = new mutable.HashSet[Point]
          }
          dm(cp) += np
          cp = np
        }
      }
    }
    dm
  }

  def move(p: Point, dir: Char): Point = dir match {
    case 'N' => (p._1, p._2 - 1)
    case 'S' => (p._1, p._2 + 1)
    case 'E' => (p._1 + 1, p._2)
    case 'W' => (p._1 - 1, p._2)
    case _ => p
  }

  def countRooms(dm: DoorMap, minDoors: Int): Int = {
    val visited = new mutable.HashMap[Point, Int]
    var queue = new mutable.Queue[Point]
    queue.enqueue((0, 0))
    var roomCount = 0

    while (queue.nonEmpty) {
      val p = queue.dequeue()
      for (np <- dm.getOrElse(p, new mutable.HashSet[Point])) {
        if (!visited.contains(np)) {
          visited(np) = visited.getOrElse(p, 0) + 1
          if (visited(np) >= minDoors) {
            roomCount += 1
          }
          queue.enqueue(np)
        }
      }
    }
    roomCount
  }
}