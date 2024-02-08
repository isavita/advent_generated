
object Day12 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  // Part One
  var x = 0
  var y = 0
  var dir = 0

  for (line <- input) {
    val action = line.head
    val value = line.tail.toInt

    action match {
      case 'N' => y += value
      case 'S' => y -= value
      case 'E' => x += value
      case 'W' => x -= value
      case 'L' => dir = (dir + value) % 360
      case 'R' => dir = (dir - value + 360) % 360
      case 'F' =>
        dir match {
          case 0 => x += value
          case 90 => y -= value
          case 180 => x -= value
          case 270 => y += value
        }
    }
  }

  val partOneAnswer = math.abs(x) + math.abs(y)
  println(partOneAnswer)

  // Part Two
  var shipX = 0
  var shipY = 0
  var wayX = 10
  var wayY = 1

  for (line <- input) {
    val action = line.head
    val value = line.tail.toInt

    action match {
      case 'N' => wayY += value
      case 'S' => wayY -= value
      case 'E' => wayX += value
      case 'W' => wayX -= value
      case 'L' =>
        for (_ <- 0 until value / 90) {
          val temp = wayX
          wayX = -wayY
          wayY = temp
        }
      case 'R' =>
        for (_ <- 0 until value / 90) {
          val temp = wayX
          wayX = wayY
          wayY = -temp
        }
      case 'F' =>
        shipX += wayX * value
        shipY += wayY * value
    }
  }

  val partTwoAnswer = math.abs(shipX) + math.abs(shipY)
  println(partTwoAnswer)
}
