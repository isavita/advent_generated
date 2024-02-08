
object Main extends App {
  val program = scala.io.Source.fromFile("input.txt").getLines.next.split(",").map(_.toInt).toArray
  def runProgram(program: Array[Int], input: Int): Int = {
    var memory = program.clone
    var ip = 0
    var output = 0
    def getValue(param: Int, mode: Int): Int = if (mode == 0) memory(param) else param
    while (memory(ip) != 99) {
      val instruction = memory(ip)
      val opcode = instruction % 100
      val modes = instruction.toString.dropRight(2).reverse.padTo(3, '0').map(_.asDigit)
      opcode match {
        case 1 =>
          val param1 = memory(ip + 1)
          val param2 = memory(ip + 2)
          val param3 = memory(ip + 3)
          memory(param3) = getValue(param1, modes(0)) + getValue(param2, modes(1))
          ip += 4
        case 2 =>
          val param1 = memory(ip + 1)
          val param2 = memory(ip + 2)
          val param3 = memory(ip + 3)
          memory(param3) = getValue(param1, modes(0)) * getValue(param2, modes(1))
          ip += 4
        case 3 =>
          val param1 = memory(ip + 1)
          memory(param1) = input
          ip += 2
        case 4 =>
          val param1 = memory(ip + 1)
          output = getValue(param1, modes(0))
          ip += 2
        case 5 =>
          val param1 = memory(ip + 1)
          val param2 = memory(ip + 2)
          if (getValue(param1, modes(0)) != 0) ip = getValue(param2, modes(1)) else ip += 3
        case 6 =>
          val param1 = memory(ip + 1)
          val param2 = memory(ip + 2)
          if (getValue(param1, modes(0)) == 0) ip = getValue(param2, modes(1)) else ip += 3
        case 7 =>
          val param1 = memory(ip + 1)
          val param2 = memory(ip + 2)
          val param3 = memory(ip + 3)
          memory(param3) = if (getValue(param1, modes(0)) < getValue(param2, modes(1))) 1 else 0
          ip += 4
        case 8 =>
          val param1 = memory(ip + 1)
          val param2 = memory(ip + 2)
          val param3 = memory(ip + 3)
          memory(param3) = if (getValue(param1, modes(0)) == getValue(param2, modes(1))) 1 else 0
          ip += 4
      }
    }
    output
  }
  println(runProgram(program, 5))
}
