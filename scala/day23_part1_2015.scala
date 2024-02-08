
import scala.io.Source

object Main extends App {
  val instructions = Source.fromFile("input.txt").getLines.toList

  var registers = Map("a" -> 0, "b" -> 0)

  var i = 0
  while (i < instructions.length) {
    val parts = instructions(i).split(" ")

    parts(0) match {
      case "hlf" => registers += (parts(1) -> (registers(parts(1)) / 2))
      case "tpl" => registers += (parts(1) -> (registers(parts(1)) * 3))
      case "inc" => registers += (parts(1) -> (registers(parts(1)) + 1))
      case "jmp" => i += parts(1).toInt - 1
      case "jie" => if (registers(parts(1).take(1)) % 2 == 0) i += parts(2).toInt - 1
      case "jio" => if (registers(parts(1).take(1)) == 1) i += parts(2).toInt - 1
      case _     => throw new IllegalArgumentException(s"Unknown instruction: ${parts(0)}")
    }

    i += 1
  }

  println(registers("b"))
}
