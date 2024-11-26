
import scala.io.Source

object TractorBeam {
  def runIntcode(program: Array[Long], inputs: List[Long]): Long = {
    var memory = program.clone()
    var inputIndex = 0
    var relativeBase = 0
    var ip = 0

    def getMemory(index: Int): Long = {
      if (index >= memory.length) {
        memory = memory ++ Array.fill(index - memory.length + 1)(0L)
      }
      memory(index)
    }

    def setMemory(index: Int, value: Long): Unit = {
      if (index >= memory.length) {
        memory = memory ++ Array.fill(index - memory.length + 1)(0L)
      }
      memory(index) = value
    }

    def getParam(mode: Int, param: Long): Long = mode match {
      case 0 => getMemory(param.toInt)
      case 1 => param
      case 2 => getMemory((relativeBase + param).toInt)
    }

    def writeParam(mode: Int, param: Long, value: Long): Unit = mode match {
      case 0 => setMemory(param.toInt, value)
      case 2 => setMemory((relativeBase + param).toInt, value)
    }

    while (memory(ip) != 99L) {
      val opcode = memory(ip) % 100
      val modes = Array(
        (memory(ip) / 100 % 10).toInt,
        (memory(ip) / 1000 % 10).toInt,
        (memory(ip) / 10000 % 10).toInt
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
          writeParam(modes(0), memory(ip + 1), inputs(inputIndex))
          inputIndex += 1
          ip += 2

        case 4 => // Output
          val output = getParam(modes(0), memory(ip + 1))
          return output

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
          relativeBase += getParam(modes(0), memory(ip + 1)).toInt
          ip += 2
      }
    }
    -1L
  }

  def checkTractorBeam(program: Array[Long], x: Int, y: Int): Boolean = {
    runIntcode(program, List(x.toLong, y.toLong)) == 1L
  }

  def countTractorBeamPoints(program: Array[Long], gridSize: Int): Int = {
    (0 until gridSize).flatMap { y =>
      (0 until gridSize).filter(x => checkTractorBeam(program, x, y))
    }.size
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim
    val program = input.split(",").map(_.toLong)

    val result = countTractorBeamPoints(program, 50)
    println(s"Points affected by tractor beam: $result")
  }
}
