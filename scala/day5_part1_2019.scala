object Solution extends App {
  import scala.io.Source

  def getMode(instruction: Int, position: Int): Int = instruction / math.pow(10, position + 1).toInt % 10

  def getParam(program: Array[Int], pointer: Int, mode: Int): Int = if (mode == 0) program(program(pointer)) else program(pointer)

  def runProgram(program: Array[Int], input: Int): Int = {
    var output = 0
    var pointer = 0
    while (pointer < program.length) {
      val instruction = program(pointer)
      val opcode = instruction % 100

      opcode match {
        case 1 | 2 =>
          val param1 = getParam(program, pointer + 1, getMode(instruction, 1))
          val param2 = getParam(program, pointer + 2, getMode(instruction, 2))
          val result = if (opcode == 1) param1 + param2 else param1 * param2
          program(program(pointer + 3)) = result
          pointer += 4
        case 3 =>
          program(program(pointer + 1)) = input
          pointer += 2
        case 4 =>
          output = getParam(program, pointer + 1, getMode(instruction, 1))
          pointer += 2
        case 99 => return output
        case _ => throw new RuntimeException(s"Unknown opcode: $opcode")
      }
    }
    output
  }

  val data = Source.fromFile("input.txt").getLines.mkString.trim
  val strProgram = data.split(",")
  val program = strProgram.map(_.toInt)

  println(runProgram(program, 1))
}