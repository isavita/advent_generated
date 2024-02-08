
object Main extends App {
  val program = scala.io.Source.fromFile("input.txt").getLines.next.split(",").map(_.toInt).toList

  def runAmplifiers(phaseSettings: List[Int]): Int = {
    val amplifiers = phaseSettings.map(phaseSetting => new IntcodeComputer(program, List(phaseSetting)))
    var inputSignal = 0
    var outputSignal = 0

    while (!amplifiers.last.halted) {
      for (amp <- amplifiers) {
        amp.addInput(inputSignal)
        amp.run()
        outputSignal = amp.getOutput
        inputSignal = outputSignal
      }
    }
    outputSignal
  }

  val phaseSettings = List(5, 6, 7, 8, 9)
  val maxSignal = phaseSettings.permutations.map(runAmplifiers).max
  println(maxSignal)
}

class IntcodeComputer(program: List[Int], initialInput: List[Int]) {
  var memory: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map(program.zipWithIndex.map(_.swap):_*)
  var input: List[Int] = initialInput
  var output: Int = 0
  var pc: Int = 0
  var halted: Boolean = false

  def addInput(newInput: Int): Unit = {
    input = input :+ newInput
  }

  def getOutput: Int = output

  def run(): Unit = {
    while (!halted) {
      val opcode = memory(pc) % 100
      val modes = memory(pc) / 100
      opcode match {
        case 1 =>
          val param1 = if (modes % 10 == 1) memory(pc + 1) else memory(memory(pc + 1))
          val param2 = if ((modes / 10) % 10 == 1) memory(pc + 2) else memory(memory(pc + 2))
          memory(memory(pc + 3)) = param1 + param2
          pc += 4
        case 2 =>
          val param1 = if (modes % 10 == 1) memory(pc + 1) else memory(memory(pc + 1))
          val param2 = if ((modes / 10) % 10 == 1) memory(pc + 2) else memory(memory(pc + 2))
          memory(memory(pc + 3)) = param1 * param2
          pc += 4
        case 3 =>
          memory(memory(pc + 1)) = input.head
          input = input.tail
          pc += 2
        case 4 =>
          output = memory(memory(pc + 1))
          pc += 2
          return
        case 5 =>
          if (memory(memory(pc + 1)) != 0) pc = memory(memory(pc + 2)) else pc += 3
        case 6 =>
          if (memory(memory(pc + 1)) == 0) pc = memory(memory(pc + 2)) else pc += 3
        case 7 =>
          memory(memory(pc + 3)) = if (memory(memory(pc + 1)) < memory(memory(pc + 2))) 1 else 0
          pc += 4
        case 8 =>
          memory(memory(pc + 3)) = if (memory(memory(pc + 1)) == memory(memory(pc + 2))) 1 else 0
          pc += 4
        case 99 =>
          halted = true
          return
      }
    }
  }
}
