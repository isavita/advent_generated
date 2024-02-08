object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  var x = 0
  var y = 0
  var direction = 0

  for (line <- input) {
    val action = line.head
    val value = line.tail.toInt

    action match {
      case 'N' => y += value
      case 'S' => y -= value
      case 'E' => x += value
      case 'W' => x -= value
      case 'L' => direction = (direction + 360 - value) % 360
      case 'R' => direction = (direction + value) % 360
      case 'F' =>
        direction match {
          case 0 => x += value
          case 90 => y -= value
          case 180 => x -= value
          case 270 => y += value
        }
    }
  }

  println(math.abs(x) + math.abs(y))
}