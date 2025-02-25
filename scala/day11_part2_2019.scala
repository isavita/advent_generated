
import scala.collection.mutable
import scala.io.Source

object Day11 {

  class IntcodeComputer(program: Array[Long]) {
    private val memory: mutable.Map[Long, Long] = mutable.Map.empty.withDefaultValue(0L)
    for ((value, index) <- program.zipWithIndex) {
      memory(index.toLong) = value
    }
    private var ip: Long = 0
    private var relativeBase: Long = 0
    var halted: Boolean = false

    private def getParameter(mode: Int, offset: Long): Long = {
      val param = memory(ip + offset)
      mode match {
        case 0 => memory(param)
        case 1 => param
        case 2 => memory(relativeBase + param)
        case _ => throw new IllegalArgumentException(s"Unknown parameter mode: $mode")
      }
    }

    private def setParameter(mode: Int, offset: Long, value: Long): Unit = {
      val param = memory(ip + offset)
      mode match {
        case 0 => memory(param) = value
        case 2 => memory(relativeBase + param) = value
        case _ => throw new IllegalArgumentException(s"Unknown parameter mode for writing: $mode")
      }
    }

    def run(inputProvider: () => Long, outputConsumer: Long => Unit): Unit = {
      while (!halted) {
        val instruction = memory(ip)
        val opcode = (instruction % 100).toInt
        val modes = Array(
          ((instruction / 100) % 10).toInt,
          ((instruction / 1000) % 10).toInt,
          ((instruction / 10000) % 10).toInt
        )

        opcode match {
          case 1 => // Addition
            val param1 = getParameter(modes(0), 1)
            val param2 = getParameter(modes(1), 2)
            setParameter(modes(2), 3, param1 + param2)
            ip += 4
          case 2 => // Multiplication
            val param1 = getParameter(modes(0), 1)
            val param2 = getParameter(modes(1), 2)
            setParameter(modes(2), 3, param1 * param2)
            ip += 4
          case 3 => // Input
            setParameter(modes(0), 1, inputProvider())
            ip += 2
          case 4 => // Output
            val outputVal = getParameter(modes(0), 1)
            outputConsumer(outputVal)
            ip += 2
          case 5 => // Jump-if-true
            val param1 = getParameter(modes(0), 1)
            val param2 = getParameter(modes(1), 2)
            if (param1 != 0) {
              ip = param2
            } else {
              ip += 3
            }
          case 6 => // Jump-if-false
            val param1 = getParameter(modes(0), 1)
            val param2 = getParameter(modes(1), 2)
            if (param1 == 0) {
              ip = param2
            } else {
              ip += 3
            }
          case 7 => // Less than
            val param1 = getParameter(modes(0), 1)
            val param2 = getParameter(modes(1), 2)
            setParameter(modes(2), 3, if (param1 < param2) 1 else 0)
            ip += 4
          case 8 => // Equals
            val param1 = getParameter(modes(0), 1)
            val param2 = getParameter(modes(1), 2)
            setParameter(modes(2), 3, if (param1 == param2) 1 else 0)
            ip += 4
          case 9 => // Adjust relative base
            val param1 = getParameter(modes(0), 1)
            relativeBase += param1
            ip += 2
          case 99 => // Halt
            halted = true
          case _ =>
            throw new IllegalArgumentException(s"Unknown opcode: $opcode")
        }
      }
    }
  }

  class Robot(program: Array[Long], startPanelColor: Int = 0) {
    private val computer = new IntcodeComputer(program.clone())
    private var direction: Int = 0 // 0: Up, 1: Right, 2: Down, 3: Left
    private var position: (Int, Int) = (0, 0) // (x, y)
    private val panels: mutable.Map[(Int, Int), Int] = mutable.Map.empty.withDefaultValue(0)
    panels(position) = startPanelColor
    private val paintedPanels: mutable.Set[(Int, Int)] = mutable.Set.empty

    def turnAndMove(turnDirection: Int): Unit = {
      turnDirection match {
        case 0 => direction = (direction - 1 + 4) % 4 // Turn left
        case 1 => direction = (direction + 1) % 4 // Turn right
        case _ => throw new IllegalArgumentException(s"Unknown turn direction: $turnDirection")
      }

      // Move forward
      position = direction match {
        case 0 => (position._1, position._2 - 1) // Up
        case 1 => (position._1 + 1, position._2) // Right
        case 2 => (position._1, position._2 + 1) // Down
        case 3 => (position._1 - 1, position._2) // Left
      }
    }

    def run(): Unit = {
      var paintColor: Option[Long] = None
      var turnDirection: Option[Long] = None

      computer.run(
        () => {
          panels(position).toLong
        },
        output => {
          if (paintColor.isEmpty) {
            paintColor = Some(output)
          } else {
            turnDirection = Some(output)

            panels(position) = paintColor.get.toInt
            paintedPanels.add(position)
            turnAndMove(turnDirection.get.toInt)

            paintColor = None
            turnDirection = None
          }
        }
      )
    }

    def getPaintedPanelsCount: Int = paintedPanels.size

    def renderPanels(): Unit = {
      if (panels.isEmpty) {
        println("No panels painted.")
        return
      }

      val minX = panels.keys.map(_._1).min
      val maxX = panels.keys.map(_._1).max
      val minY = panels.keys.map(_._2).min
      val maxY = panels.keys.map(_._2).max

      println("\nRegistration Identifier:")
      for (y <- minY to maxY) {
        val row = new StringBuilder()
        for (x <- minX to maxX) {
          if (panels.getOrElse((x, y), 0) == 1) {
            row.append('#')
          } else {
            row.append(' ')
          }
        }
        println(row.toString())
      }
    }
  }

  def parseInput(filePath: String): Array[Long] = {
    val source = Source.fromFile(filePath)
    val program = source.getLines().next().split(",").map(_.toLong)
    source.close()
    program
  }

  def main(args: Array[String]): Unit = {
    val inputFile = "input.txt"
    val program = parseInput(inputFile)

    // Part One
    val robotPart1 = new Robot(program, startPanelColor = 0)
    robotPart1.run()
    val paintedCountPart1 = robotPart1.getPaintedPanelsCount
    println(s"Part One: $paintedCountPart1 panels painted at least once.")

    // Part Two
    val robotPart2 = new Robot(program, startPanelColor = 1)
    robotPart2.run()
    println("Part Two: Registration identifier painted on the hull.")
    robotPart2.renderPanels()
  }
}
