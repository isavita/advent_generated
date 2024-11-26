
import scala.io.Source

object IntcodeComputer {
  def runProgram(program: Array[Long], input: Long): Long = {
    var memory = program.padTo(10000, 0L)
    var relativeBase = 0L
    var ip = 0
    var output = 0L

    def getParam(mode: Int, param: Long): Long = mode match {
      case 0 => memory(param.toInt)
      case 1 => param
      case 2 => memory((relativeBase + param).toInt)
    }

    def writeParam(mode: Int, param: Long, value: Long): Unit = mode match {
      case 0 => memory(param.toInt) = value
      case 2 => memory((relativeBase + param).toInt) = value
    }

    while (memory(ip) != 99L) {
      val instruction = memory(ip)
      val opcode = instruction % 100
      val modes = Array(
        (instruction / 100 % 10).toInt,
        (instruction / 1000 % 10).toInt,
        (instruction / 10000 % 10).toInt
      )

      opcode match {
        case 1 => // Addition
          val result = getParam(modes(0), memory(ip + 1)) + getParam(modes(1), memory(ip + 2))
          writeParam(modes(2), memory(ip + 3), result)
          ip += 4

        case 2 => // Multiplication
          val result = getParam(modes(0), memory(ip + 1)) * getParam(modes(1), memory(ip + 2))
          writeParam(modes(2), memory(ip + 3), result)
          ip += 4

        case 3 => // Input
          writeParam(modes(0), memory(ip + 1), input)
          ip += 2

        case 4 => // Output
          output = getParam(modes(0), memory(ip + 1))
          ip += 2

        case 5 => // Jump-if-true
          ip = if (getParam(modes(0), memory(ip + 1)) != 0) getParam(modes(1), memory(ip + 2)).toInt else ip + 3

        case 6 => // Jump-if-false
          ip = if (getParam(modes(0), memory(ip + 1)) == 0) getParam(modes(1), memory(ip + 2)).toInt else ip + 3

        case 7 => // Less than
          val result = if (getParam(modes(0), memory(ip + 1)) < getParam(modes(1), memory(ip + 2))) 1L else 0L
          writeParam(modes(2), memory(ip + 3), result)
          ip += 4

        case 8 => // Equals
          val result = if (getParam(modes(0), memory(ip + 1)) == getParam(modes(1), memory(ip + 2))) 1L else 0L
          writeParam(modes(2), memory(ip + 3), result)
          ip += 4

        case 9 => // Adjust relative base
          relativeBase += getParam(modes(0), memory(ip + 1))
          ip += 2
      }
    }
    output
  }

  def main(args: Array[String]): Unit = {
    val program = Source.fromFile("input.txt").mkString.trim
      .split(",")
      .map(_.toLong)

    // Part 1: Test mode
    val boostKeycode = runProgram(program.clone(), 1)
    println(s"BOOST Keycode: $boostKeycode")

    // Part 2: Sensor boost mode
    val coordinates = runProgram(program.clone(), 2)
    println(s"Distress Signal Coordinates: $coordinates")
  }
}
