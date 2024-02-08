
object Main extends App {
  import scala.io.Source

  val input = Source.fromFile("input.txt").getLines().toList

  def getValue(registers: Map[Char, Int], value: String): Int = {
    if (value.head.isLetter) registers(value.head)
    else value.toInt
  }

  def runProgram(instructions: List[String], registers: Map[Char, Int], pc: Int): Map[Char, Int] = {
    if (pc >= instructions.length) registers
    else {
      val parts = instructions(pc).split(" ")
      parts(0) match {
        case "cpy" =>
          val value = getValue(registers, parts(1))
          val dest = parts(2)(0)
          runProgram(instructions, registers + (dest -> value), pc + 1)
        case "inc" =>
          val reg = parts(1)(0)
          runProgram(instructions, registers + (reg -> (registers(reg) + 1)), pc + 1)
        case "dec" =>
          val reg = parts(1)(0)
          runProgram(instructions, registers + (reg -> (registers(reg) - 1)), pc + 1)
        case "jnz" =>
          val check = getValue(registers, parts(1))
          val offset = getValue(registers, parts(2))
          if (check != 0) runProgram(instructions, registers, pc + offset)
          else runProgram(instructions, registers, pc + 1)
      }
    }
  }

  val initialRegisters = Map('a' -> 0, 'b' -> 0, 'c' -> 0, 'd' -> 0)
  val finalRegisters = runProgram(input, initialRegisters, 0)
  println(finalRegisters('a'))
}
