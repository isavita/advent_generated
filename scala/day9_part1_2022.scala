object Solution extends App {
  import scala.io.Source

  case class Point(x: Int, y: Int)

  val file = Source.fromFile("input.txt")
  val lines = file.getLines().toList
  file.close()

  var head = Point(0, 0)
  var tail = Point(0, 0)
  var visited = Map(tail -> true)

  lines.foreach { line =>
    val Array(dir, steps) = line.split(" ")
    val numSteps = steps.toInt

    (1 to numSteps).foreach { _ =>
      dir match {
        case "R" => head = head.copy(x = head.x + 1)
        case "L" => head = head.copy(x = head.x - 1)
        case "U" => head = head.copy(y = head.y + 1)
        case "D" => head = head.copy(y = head.y - 1)
      }

      if (math.abs(head.x - tail.x) > 1 || math.abs(head.y - tail.y) > 1) {
        if (head.x != tail.x && head.y != tail.y) {
          tail = Point(if (head.x > tail.x) tail.x + 1 else tail.x - 1, if (head.y > tail.y) tail.y + 1 else tail.y - 1)
        } else {
          tail = Point(if (head.x > tail.x) tail.x + 1 else if (head.x < tail.x) tail.x - 1 else tail.x, if (head.y > tail.y) tail.y + 1 else if (head.y < tail.y) tail.y - 1 else tail.y)
        }
      }

      visited += (tail -> true)
    }
  }

  println(visited.size)
}