
import scala.io.Source
import scala.collection.mutable

object AmplificationCircuit {
  def runIntcode(program: Array[Int], inputs: mutable.Queue[Int]): Int = {
    val memory = program.clone()
    var ip = 0
    var output = 0

    while (ip < memory.length) {
      val opcode = memory(ip) % 100
      val modes = Array(
        (memory(ip) / 100) % 10,
        (memory(ip) / 1000) % 10,
        (memory(ip) / 10000) % 10
      )

      def getParam(index: Int): Int = {
        if (modes(index - 1) == 1) memory(ip + index)
        else memory(memory(ip + index))
      }

      opcode match {
        case 1 => // Addition
          memory(memory(ip + 3)) = getParam(1) + getParam(2)
          ip += 4
        case 2 => // Multiplication
          memory(memory(ip + 3)) = getParam(1) * getParam(2)
          ip += 4
        case 3 => // Input
          memory(memory(ip + 1)) = inputs.dequeue()
          ip += 2
        case 4 => // Output
          output = getParam(1)
          ip += 2
        case 5 => // Jump-if-true
          ip = if (getParam(1) != 0) getParam(2) else ip + 3
        case 6 => // Jump-if-false
          ip = if (getParam(1) == 0) getParam(2) else ip + 3
        case 7 => // Less than
          memory(memory(ip + 3)) = if (getParam(1) < getParam(2)) 1 else 0
          ip += 4
        case 8 => // Equals
          memory(memory(ip + 3)) = if (getParam(1) == getParam(2)) 1 else 0
          ip += 4
        case 99 => // Halt
          return output
      }
    }
    output
  }

  def findMaxThrusterSignal(program: Array[Int]): Int = {
    val phaseSettings = (0 to 4).permutations

    phaseSettings.map { settings =>
      var signal = 0
      for (phase <- settings) {
        val inputs = mutable.Queue(phase, signal)
        signal = runIntcode(program, inputs)
      }
      signal
    }.max
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim
    val program = input.split(",").map(_.toInt)
    val maxSignal = findMaxThrusterSignal(program)
    println(s"Max thruster signal: $maxSignal")
  }
}
