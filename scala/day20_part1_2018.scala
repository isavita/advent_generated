import scala.io.Source

case class Point(x: Int, y: Int)
type DoorMap = Map[Point, Map[Point, Boolean]]

object Main {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("input.txt").mkString
    val regex = data.substring(1, data.length - 1)
    val dm = buildMap(regex)
    val maxDoors = findFurthestRoom(dm)
    println(maxDoors)
  }

  def buildMap(regex: String): DoorMap = {
    var dm: DoorMap = Map.empty
    var stack: List[Point] = Nil
    var cp = Point(0, 0)
    for (c <- regex) {
      c match {
        case '(' => stack = cp :: stack
        case '|' => cp = stack.head
        case ')' => {
          cp = stack.head
          stack = stack.tail
        }
        case _ => {
          val np = move(cp, c)
          if (!dm.contains(cp)) dm += (cp -> Map.empty)
          dm += (cp -> (dm(cp) + (np -> true)))
          cp = np
        }
      }
    }
    dm
  }

  def move(p: Point, dir: Char): Point = dir match {
    case 'N' => Point(p.x, p.y - 1)
    case 'S' => Point(p.x, p.y + 1)
    case 'E' => Point(p.x + 1, p.y)
    case 'W' => Point(p.x - 1, p.y)
    case _ => p
  }

  def findFurthestRoom(dm: DoorMap): Int = {
    var visited: Map[Point, Int] = Map.empty
    var queue: List[Point] = List(Point(0, 0))
    var maxDoors = 0

    while (queue.nonEmpty) {
      val p = queue.head
      queue = queue.tail
      for (np <- dm.getOrElse(p, Map.empty).keys) {
        if (!visited.contains(np)) {
          visited += (np -> (visited.getOrElse(p, 0) + 1))
          maxDoors = math.max(maxDoors, visited(np))
          queue = np :: queue
        }
      }
    }
    maxDoors
  }
}