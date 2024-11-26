
object LavaductLagoon {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().toList
    
    // Part 1
    val part1Result = calculateLagoonArea(parseInput(input, isPart2 = false))
    println(s"Part 1: $part1Result")
    
    // Part 2
    val part2Result = calculateLagoonArea(parseInput(input, isPart2 = true))
    println(s"Part 2: $part2Result")
  }
  
  def parseInput(input: List[String], isPart2: Boolean): List[(String, Long)] = {
    input.map { line =>
      val parts = line.split(" ")
      if (!isPart2) {
        (parts(0), parts(1).toLong)
      } else {
        val hexCode = parts(2).drop(2).dropRight(1)
        val distance = Integer.parseInt(hexCode.take(5), 16)
        val direction = hexCode.last match {
          case '0' => "R"
          case '1' => "D"
          case '2' => "L"
          case '3' => "U"
        }
        (direction, distance)
      }
    }
  }
  
  def calculateLagoonArea(instructions: List[(String, Long)]): Long = {
    var x = 0L
    var y = 0L
    var boundary = 0L
    val vertices = instructions.foldLeft(List[(Long, Long)]((0, 0))) { (acc, instruction) =>
      val (dir, dist) = instruction
      val (dx, dy) = dir match {
        case "R" => (1L, 0L)
        case "L" => (-1L, 0L)
        case "U" => (0L, -1L)
        case "D" => (0L, 1L)
      }
      
      x += dx * dist
      y += dy * dist
      boundary += dist
      
      acc :+ (x, y)
    }
    
    // Shoelace formula (Pick's theorem)
    val area = math.abs(vertices.sliding(2).map { case List((x1, y1), (x2, y2)) =>
      x1 * y2 - x2 * y1
    }.sum / 2)
    
    // Add boundary points
    area + boundary / 2 + 1
  }
}
