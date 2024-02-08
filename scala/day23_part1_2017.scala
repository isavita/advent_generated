object Solution extends App {
  import scala.io.Source
  import scala.util.Try

  var mulCount = 0
  var pointer = 0
  var registers = Map[String, Int]()
  var instructions = List[String]()

  val file = Source.fromFile("input.txt")
  for (line <- file.getLines()) {
    instructions = instructions :+ line
  }
  file.close()

  def getValue(s: String): Int = {
    Try(s.toInt).getOrElse(registers.getOrElse(s, 0))
  }

  while (pointer >= 0 && pointer < instructions.length) {
    val parts = instructions(pointer).split(" ")
    val cmd = parts(0)
    val x = parts(1)
    val y = parts(2)

    cmd match {
      case "set" =>
        registers += (x -> getValue(y))
      case "sub" =>
        registers += (x -> (registers.getOrElse(x, 0) - getValue(y)))
      case "mul" =>
        registers += (x -> (registers.getOrElse(x, 0) * getValue(y)))
        mulCount += 1
      case "jnz" =>
        if (getValue(x) != 0) {
          pointer += getValue(y) - 1
        }
    }
    pointer += 1
  }

  println(mulCount)
}