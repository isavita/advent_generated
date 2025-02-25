
import scala.collection.mutable
import scala.io.Source

object IntcodeComputer {
  def main(args: Array[String]): Unit = {
    val program = Source.fromFile("input.txt").mkString.trim.split(',').map(_.toLong)

    val computers = (0 until 50).map { address =>
      new IntcodeComputer(program.clone(), mutable.Queue(address.toLong))
    }

    val packetQueues = Array.fill(50)(mutable.Queue[(Long, Long)]())
    var firstPacketTo255: Option[Long] = None

    while (firstPacketTo255.isEmpty) {
      var idle = true
      for (i <- 0 until 50) {
        val computer = computers(i)
        if (packetQueues(i).nonEmpty) {
          val (x, y) = packetQueues(i).dequeue()
          computer.inputs.enqueue(x, y)
        } else {
          computer.inputs.enqueue(-1)
        }

        computer.run()

        while (computer.outputs.length >= 3) {
          idle = false
          val dest = computer.outputs.dequeue().toInt
          val x = computer.outputs.dequeue()
          val y = computer.outputs.dequeue()

          if (dest == 255 && firstPacketTo255.isEmpty) {
            firstPacketTo255 = Some(y)
          } else if (0 <= dest && dest < 50) {
            packetQueues(dest).enqueue((x, y))
          }
        }
      }
    }
      println(firstPacketTo255.get)
  }
}

class IntcodeComputer(program: Array[Long], val inputs: mutable.Queue[Long]) {
  val memory: mutable.Map[Long, Long] = mutable.Map.from(program.zipWithIndex.map { case (v, i) => (i.toLong, v) })
  var ip: Long = 0
  var relativeBase: Long = 0
  val outputs: mutable.Queue[Long] = mutable.Queue()
  var halted: Boolean = false
  var needsInput: Boolean = false

  def getParam(mode: Int, offset: Int): Long = {
    mode match {
      case 0 => memory.getOrElse(memory.getOrElse(ip + offset, 0L), 0L)
      case 1 => memory.getOrElse(ip + offset, 0L)
      case 2 => memory.getOrElse(relativeBase + memory.getOrElse(ip + offset, 0L), 0L)
    }
  }

  def setParam(mode: Int, offset: Int, value: Long): Unit = {
    mode match {
      case 0 => memory(memory.getOrElse(ip + offset, 0L)) = value
      case 2 => memory(relativeBase + memory.getOrElse(ip + offset, 0L)) = value
    }
  }

  def run(): Unit = {
    while (!halted) {
      val opcode = (memory.getOrElse(ip, 0L) % 100).toInt
      val modes = Seq(
        (memory.getOrElse(ip, 0L) / 100 % 10).toInt,
        (memory.getOrElse(ip, 0L) / 1000 % 10).toInt,
        (memory.getOrElse(ip, 0L) / 10000 % 10).toInt
      )

      opcode match {
        case 99 => halted = true
        case 1 | 2 | 7 | 8 =>
          val param1 = getParam(modes(0), 1)
          val param2 = getParam(modes(1), 2)
          val result = opcode match {
            case 1 => param1 + param2
            case 2 => param1 * param2
            case 7 => if (param1 < param2) 1L else 0L
            case 8 => if (param1 == param2) 1L else 0L
          }
          setParam(modes(2), 3, result)
          ip += 4
        case 3 =>
          if (inputs.isEmpty) {
            needsInput = true
            return
          }
          needsInput = false
          setParam(modes(0), 1, inputs.dequeue())
          ip += 2
        case 4 =>
          outputs.enqueue(getParam(modes(0), 1))
          ip += 2
          if (outputs.length == 3) return
        case 5 | 6 =>
          val param1 = getParam(modes(0), 1)
          val param2 = getParam(modes(1), 2)
          ip = if ((opcode == 5 && param1 != 0) || (opcode == 6 && param1 == 0)) param2 else ip + 3
        case 9 =>
          relativeBase += getParam(modes(0), 1)
          ip += 2
        case _ => throw new Exception(s"Unknown opcode $opcode")
      }
    }
  }
}
