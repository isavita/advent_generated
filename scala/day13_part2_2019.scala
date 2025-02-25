
import scala.collection.mutable
import scala.io.Source

object Arcade {

  class IntcodeComputer(program: Array[Long]) {
    val memory: mutable.Map[Long, Long] = mutable.Map.empty.withDefaultValue(0L)
    for (i <- program.indices) {
      memory(i.toLong) = program(i)
    }
    var ip: Long = 0
    var relativeBase: Long = 0
    var awaitingInput: Boolean = false

    def getParameter(offset: Int, mode: Int): Long = {
      mode match {
        case 0 => memory(memory(ip + offset))
        case 1 => memory(ip + offset)
        case 2 => memory(relativeBase + memory(ip + offset))
      }
    }

    def getWriteAddress(offset: Int, mode: Int): Long = {
      mode match {
        case 0 => memory(ip + offset)
        case 2 => relativeBase + memory(ip + offset)
      }
    }

    def execute(inputProvider: () => Long): Iterator[Long] = new Iterator[Long] {
      var nextOutput: Option[Long] = None

      override def hasNext: Boolean = {
        if (nextOutput.isDefined) {
          true
        } else {
          computeNext()
          nextOutput.isDefined
        }
      }

      override def next(): Long = {
        if (!hasNext) throw new NoSuchElementException
        val value = nextOutput.get
        nextOutput = None
        value
      }

      private def computeNext(): Unit = {
        while (nextOutput.isEmpty) {
          val instruction = memory(ip)
          val opcode = (instruction % 100).toInt
          val modes = Seq.tabulate(3)(i => ((instruction / math.pow(10, 2 + i)).toLong % 10).toInt)

          opcode match {
            case 99 => return // Program complete
            case 3 => // Input
              if (!awaitingInput) {
                val inputValue = inputProvider()
                memory(getWriteAddress(1, modes(0))) = inputValue
                awaitingInput = true
              }
              ip += 2
            case 4 => // Output
              val outputValue = getParameter(1, modes(0))
              ip += 2
              awaitingInput = false
              nextOutput = Some(outputValue)
            case _ =>
              val param1 = getParameter(1, modes(0))
              val param2 = if (!Seq(3, 4, 9).contains(opcode)) Some(getParameter(2, modes(1))) else None

              opcode match {
                case 1 => // Add
                  memory(getWriteAddress(3, modes(2))) = param1 + param2.get
                  ip += 4
                case 2 => // Multiply
                  memory(getWriteAddress(3, modes(2))) = param1 * param2.get
                  ip += 4
                case 5 => // Jump-if-true
                  if (param1 != 0) ip = param2.get else ip += 3
                case 6 => // Jump-if-false
                  if (param1 == 0) ip = param2.get else ip += 3
                case 7 => // Less than
                  memory(getWriteAddress(3, modes(2))) = if (param1 < param2.get) 1 else 0
                  ip += 4
                case 8 => // Equals
                  memory(getWriteAddress(3, modes(2))) = if (param1 == param2.get) 1 else 0
                  ip += 4
                case 9 => // Adjust relative base
                  relativeBase += param1
                  ip += 2
              }
          }
        }
      }
    }
  }

  def parseInput(filePath: String): Array[Long] = {
    Source.fromFile(filePath).getLines().next().split(",").map(_.toLong)
  }

  def playGame(program: Array[Long]): Long = {
    val computer = new IntcodeComputer(program)
    computer.memory(0) = 2 // Set to free play mode
    var score: Long = 0
    var ballX: Long = 0
    var paddleX: Long = 0

    val outputs = computer.execute(() => (ballX > paddleX).compare(ballX < paddleX).toLong)

    while (outputs.hasNext) {
      val x = outputs.next()
      val y = outputs.next()
      val tileId = outputs.next()

      if (x == -1 && y == 0) {
        score = tileId
      } else {
        if (tileId == 3) {
          paddleX = x
        } else if (tileId == 4) {
          ballX = x
        }
      }
    }

    score
  }

  def main(args: Array[String]): Unit = {
    val program = parseInput("input.txt")
    println("Final Score: " + playGame(program))
  }
}
