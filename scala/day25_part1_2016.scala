object Day25 extends App{
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  def getValue(registers: Map[Char, Int], value: String): Int = {
    if (value.head.isLetter) registers(value.head)
    else value.toInt
  }

  def runCode(instructions: List[String], registers: Map[Char, Int], index: Int, output: List[Int]): List[Int] = {
    if (index >= instructions.length || output.length >= 20) output
    else {
      val parts = instructions(index).split(" ")
      parts(0) match {
        case "cpy" => {
          val value = getValue(registers, parts(1))
          val dest = parts(2).head
          runCode(instructions, registers + (dest -> value), index + 1, output)
        }
        case "inc" => {
          val dest = parts(1).head
          runCode(instructions, registers + (dest -> (registers(dest) + 1)), index + 1, output)
        }
        case "dec" => {
          val dest = parts(1).head
          runCode(instructions, registers + (dest -> (registers(dest) - 1)), index + 1, output)
        }
        case "jnz" => {
          val check = getValue(registers, parts(1))
          if (check != 0) runCode(instructions, registers, index + parts(2).toInt, output)
          else runCode(instructions, registers, index + 1, output)
        }
        case "out" => {
          val value = getValue(registers, parts(1))
          runCode(instructions, registers, index + 1, output :+ value)
        }
      }
    }
  }

  def findClockSignal(instructions: List[String], value: Int): Int = {
    if (runCode(instructions, Map('a' -> value, 'b' -> 0, 'c' -> 0, 'd' -> 0), 0, List()).grouped(2).forall(pair => pair.head == 0 && pair(1) == 1)) value
    else findClockSignal(instructions, value + 1)
  }

  val result = findClockSignal(input, 0)
  println(result)
}