
import scala.collection.mutable
import scala.io.Source

object Day15 {

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val program = Source.fromFile(filename).getLines().mkString.split(",").map(_.toLong)
    val (minSteps, _) = findOxygenSystem(program)
    println(minSteps)
  }

  def findOxygenSystem(program: Array[Long]): (Int, Map[(Int, Int), Int]) = {
    val grid = mutable.Map[(Int, Int), Int]().withDefaultValue(-1) // -1: unexplored, 0: wall, 1: path, 2: oxygen
    val queue = mutable.Queue[((Int, Int), Int, IntcodeComputer)]() // (position, steps, computer)
    val startComputer = new IntcodeComputer(program.clone()) // Start a fresh computer
    queue.enqueue(((0, 0), 0, startComputer))
    grid((0, 0)) = 1 // Mark starting position as visited

    var minStepsToOxygen = Int.MaxValue
    var finalGrid = Map[(Int, Int), Int]()
    val directions = Map(1 -> (0, -1), 2 -> (0, 1), 3 -> (-1, 0), 4 -> (1, 0)) // (dx, dy)

    while (queue.nonEmpty) {
      val ((x, y), steps, computer) = queue.dequeue()

      for ((direction, (dx, dy)) <- directions) {
        val newX = x + dx
        val newY = y + dy

        if (grid((newX, newY)) == -1) { // Only explore unexplored cells
          val newComputer = computer.copy()
          newComputer.addInput(direction)
          newComputer.run()
          val status = newComputer.getOutput().toInt

          grid((newX, newY)) = status // Update the grid

          status match {
            case 0 => // Wall, don't add to queue
            case 1 => // Moved, add to queue
              queue.enqueue(((newX, newY), steps + 1, newComputer))
            case 2 => // Oxygen System found
              minStepsToOxygen = steps + 1
              finalGrid = grid.toMap
              queue.clear() // Optimization : we found path and no need to explore farther
          }
        }
      }
    }
    (minStepsToOxygen, finalGrid)
  }

  class IntcodeComputer(memory: Array[Long], var relativeBase: Long = 0) {
    private val mem: mutable.Map[Long, Long] = mutable.Map[Long, Long]() ++ memory.zipWithIndex.map { case (v, i) => i.toLong -> v }
    private var ip: Long = 0
    private val inputQueue: mutable.Queue[Long] = mutable.Queue[Long]()
    private var output: Long = 0

    def addInput(value: Long): Unit = inputQueue.enqueue(value)
    def getOutput(): Long = output
    def hasOutput:Boolean =  output != 0

    def run(): Unit = {
        var continue = true;
      while (continue) {
        val opcode = mem(ip) % 100
        opcode match {
          case 1 => // Add
            val (p1, p2, p3) = (getParameter(1), getParameter(2), getParameterAddress(3))
            mem(p3) = p1 + p2
            ip += 4
          case 2 => // Multiply
            val (p1, p2, p3) = (getParameter(1), getParameter(2), getParameterAddress(3))
            mem(p3) = p1 * p2
            ip += 4
          case 3 => // Input
            if (inputQueue.isEmpty) {
                continue = false;  // Pause execution if no input available
            } else {
                val p1 = getParameterAddress(1)
                mem(p1) = inputQueue.dequeue()
                ip += 2
            }

          case 4 => // Output
            output = getParameter(1)
            ip += 2
            continue = false; // Pause execution after producing output

          case 5 => // Jump-if-true
            val (p1, p2) = (getParameter(1), getParameter(2))
            ip = if (p1 != 0) p2 else ip + 3
          case 6 => // Jump-if-false
            val (p1, p2) = (getParameter(1), getParameter(2))
            ip = if (p1 == 0) p2 else ip + 3
          case 7 => // Less than
            val (p1, p2, p3) = (getParameter(1), getParameter(2), getParameterAddress(3))
            mem(p3) = if (p1 < p2) 1 else 0
            ip += 4
          case 8 => // Equals
            val (p1, p2, p3) = (getParameter(1), getParameter(2), getParameterAddress(3))
            mem(p3) = if (p1 == p2) 1 else 0
            ip += 4
          case 9 => // Adjust relative base
            relativeBase += getParameter(1)
            ip += 2
          case 99 => // Halt
            return
          case _ => throw new IllegalArgumentException(s"Invalid opcode: $opcode")
        }
      }
    }

    private def getParameter(paramNumber: Int): Long = {
      val mode = (mem(ip) / math.pow(10, paramNumber + 1).toInt) % 10
      mode match {
        case 0 => mem.getOrElse(mem.getOrElse(ip + paramNumber, 0L), 0L) // Position mode
        case 1 => mem.getOrElse(ip + paramNumber, 0L) // Immediate mode
        case 2 => mem.getOrElse(relativeBase + mem.getOrElse(ip + paramNumber, 0L), 0L) // Relative mode
        case _ => throw new IllegalArgumentException(s"Invalid parameter mode: $mode")
      }
    }

    private def getParameterAddress(paramNumber: Int): Long = {
      val mode = (mem(ip) / math.pow(10, paramNumber + 1).toInt) % 10
        mode match {
          case 0 => mem.getOrElse(ip + paramNumber, 0L)   // Position mode
          case 2 => relativeBase + mem.getOrElse(ip + paramNumber, 0L)  // Relative mode
          case _ => throw new IllegalArgumentException(s"Invalid output parameter mode: $mode, for instruction ${mem(ip)}")
        }
    }

    def copy(): IntcodeComputer = {
      val newComputer = new IntcodeComputer(memory, relativeBase)
      newComputer.mem.clear()
      newComputer.mem ++= this.mem
      newComputer.ip = this.ip
      newComputer.inputQueue ++= this.inputQueue
      newComputer.output = this.output
      newComputer
    }
  }
}
