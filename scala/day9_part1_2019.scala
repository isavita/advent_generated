
object Main extends App {
  val program = scala.io.Source.fromFile("input.txt").getLines.next.split(",").map(_.toLong).toArray
  val memory = scala.collection.mutable.Map((0L to program.length.toLong - 1L).zip(program):_*)
  var pc = 0L
  var relativeBase = 0L

  def getValue(param: Long, mode: Int): Long = mode match {
    case 0 => memory.getOrElse(param, 0L)
    case 1 => param
    case 2 => memory.getOrElse(relativeBase + param, 0L)
  }

  def setValue(param: Long, value: Long, mode: Int): Unit = mode match {
    case 0 => memory(param) = value
    case 2 => memory(relativeBase + param) = value
  }

  def runProgram(input: Long): Long = {
    var output = 0L
    var inputUsed = false

    while (true) {
      val instruction = memory(pc).toString.reverse.padTo(5, '0').reverse
      val opcode = instruction.takeRight(2).toInt
      val modes = instruction.dropRight(2).map(_.asDigit).reverse

      opcode match {
        case 1 =>
          val param1 = getValue(memory(pc + 1), modes(0))
          val param2 = getValue(memory(pc + 2), modes(1))
          setValue(memory(pc + 3), param1 + param2, modes(2))
          pc += 4

        case 2 =>
          val param1 = getValue(memory(pc + 1), modes(0))
          val param2 = getValue(memory(pc + 2), modes(1))
          setValue(memory(pc + 3), param1 * param2, modes(2))
          pc += 4

        case 3 =>
          if (!inputUsed) {
            setValue(memory(pc + 1), input, modes(0))
            inputUsed = true
          } else {
            return output
          }
          pc += 2

        case 4 =>
          output = getValue(memory(pc + 1), modes(0))
          pc += 2

        case 5 =>
          if (getValue(memory(pc + 1), modes(0)) != 0) {
            pc = getValue(memory(pc + 2), modes(1))
          } else {
            pc += 3
          }

        case 6 =>
          if (getValue(memory(pc + 1), modes(0)) == 0) {
            pc = getValue(memory(pc + 2), modes(1))
          } else {
            pc += 3
          }

        case 7 =>
          val param1 = getValue(memory(pc + 1), modes(0))
          val param2 = getValue(memory(pc + 2), modes(1))
          setValue(memory(pc + 3), if (param1 < param2) 1 else 0, modes(2))
          pc += 4

        case 8 =>
          val param1 = getValue(memory(pc + 1), modes(0))
          val param2 = getValue(memory(pc + 2), modes(1))
          setValue(memory(pc + 3), if (param1 == param2) 1 else 0, modes(2))
          pc += 4

        case 9 =>
          relativeBase += getValue(memory(pc + 1), modes(0))
          pc += 2

        case 99 => return output
      }
    }

    output
  }

  println(runProgram(1))
}
