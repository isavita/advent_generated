
object Day1 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString.split(", ")
  
  var x = 0
  var y = 0
  var dir = 0
  
  for (move <- input) {
    val turn = move.head
    val dist = move.tail.toInt
    
    dir = (dir + (if (turn == 'R') 1 else 3)) % 4
    
    dir match {
      case 0 => y += dist
      case 1 => x += dist
      case 2 => y -= dist
      case 3 => x -= dist
    }
  }
  
  println(math.abs(x) + math.abs(y))
}
