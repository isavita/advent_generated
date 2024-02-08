
object Main extends App {
  import scala.io.Source

  val input = Source.fromFile("input.txt").getLines().toList

  var registers = collection.mutable.Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0)
  var pc = 0

  def getValue(x: String): Int = if (x.head.isDigit || x(0) == '-') x.toInt else registers(x)

  while (pc < input.length) {
    val parts = input(pc).split(" ")
    parts(0) match {
      case "cpy" => registers(parts(2)) = getValue(parts(1))
      case "inc" => registers(parts(1)) += 1
      case "dec" => registers(parts(1)) -= 1
      case "jnz" => if (getValue(parts(1)) != 0) pc += getValue(parts(2)) - 1
    }
    pc += 1
  }

  println(registers("a"))
}
