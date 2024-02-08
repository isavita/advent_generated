
object Submarine {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().toList
    val commands = input.map(_.split(" "))
    
    var horizontalPosition = 0
    var depth = 0
    var aim = 0
    
    for (command <- commands) {
      val direction = command(0)
      val value = command(1).toInt
      
      if (direction == "forward") {
        horizontalPosition += value
        depth += aim * value
      } else if (direction == "down") {
        aim += value
      } else if (direction == "up") {
        aim -= value
      }
    }
    
    println(horizontalPosition * depth)
  }
}
