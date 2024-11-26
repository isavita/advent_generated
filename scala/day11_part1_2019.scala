
import scala.collection.mutable

object SpacePolice {
  // Directions: 0 = Up, 1 = Right, 2 = Down, 3 = Left
  val directions = Array((0, -1), (1, 0), (0, 1), (-1, 0))

  def runRobot(program: Array[Long]): Int = {
    val panels = mutable.Map[(Int, Int), Int]().withDefaultValue(0)
    var currentPos = (0, 0)
    var currentDirection = 0

    val computer = new IntcodeComputer(program.clone())

    while (!computer.halted) {
      // Provide current panel color as input
      computer.addInput(panels(currentPos))

      // Run until we get two outputs
      val paintColor = computer.runUntilOutput().toInt
      val turnDirection = computer.runUntilOutput().toInt

      // Paint current panel
      panels(currentPos) = paintColor

      // Turn and move
      currentDirection = (currentDirection + (if (turnDirection == 0) -1 else 1) + 4) % 4
      currentPos = (
        currentPos._1 + directions(currentDirection)._1,
        currentPos._2 + directions(currentDirection)._2
      )
    }

    // Return number of unique panels painted
    panels.size
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").mkString.trim
    val program = input.split(",").map(_.toLong)
    
    val panelsPainted = runRobot(program)
    println(s"Panels painted at least once: $panelsPainted")
  }
}

class IntcodeComputer(
  var memory: Array[Long],
  var ip: Long = 0,
  var relativeBase: Long = 0,
  var halted: Boolean = false
) {
  private val inputs = mutable.Queue[Long]()
  private val extendedMemory = mutable.Map[Long, Long]()

  def addInput(value: Long): Unit = inputs.enqueue(value)

  def getMemory(address: Long): Long = {
    if (address < memory.length) memory(address.toInt)
    else extendedMemory.getOrElse(address, 0L)
  }

  def setMemory(address: Long, value: Long): Unit = {
    if (address < memory.length) memory(address.toInt) = value
    else extendedMemory(address) = value
  }

  def runUntilOutput(): Long = {
    while (!halted) {
      val opcode = getMemory(ip) % 100
      val modes = Array(
        (getMemory(ip) / 100 % 10).toInt,
        (getMemory(ip) / 1000 % 10).toInt,
        (getMemory(ip) / 10000 % 10).toInt
      )

      def getParam(index: Int): Long = {
        val value = getMemory(ip + index + 1)
        modes(index) match {
          case 0 => getMemory(value)
          case 1 => value
          case 2 => getMemory(value + relativeBase)
        }
      }

      def getWriteAddress(index: Int): Long = {
        val value = getMemory(ip + index + 1)
        modes(index) match {
          case 0 => value
          case 2 => value + relativeBase
        }
      }

      opcode match {
        case 1 => // Add
          setMemory(getWriteAddress(2), getParam(0) + getParam(1))
          ip += 4
        case 2 => // Multiply
          setMemory(getWriteAddress(2), getParam(0) * getParam(1))
          ip += 4
        case 3 => // Input
          if (inputs.isEmpty) return 0
          setMemory(getWriteAddress(0), inputs.dequeue())
          ip += 2
        case 4 => // Output
          val output = getParam(0)
          ip += 2
          return output
        case 5 => // Jump-if-true
          ip = if (getParam(0) != 0) getParam(1) else ip + 3
        case 6 => // Jump-if-false
          ip = if (getParam(0) == 0) getParam(1) else ip + 3
        case 7 => // Less than
          setMemory(getWriteAddress(2), if (getParam(0) < getParam(1)) 1 else 0)
          ip += 4
        case 8 => // Equals
          setMemory(getWriteAddress(2), if (getParam(0) == getParam(1)) 1 else 0)
          ip += 4
        case 9 => // Adjust relative base
          relativeBase += getParam(0)
          ip += 2
        case 99 => // Halt
          halted = true
          return 0
        case _ => throw new Exception(s"Invalid opcode: $opcode")
      }
    }
    0
  }
}
