
import scala.collection.mutable
import scala.io.Source

object IntcodeComputer {
  def main(args: Array[String]): Unit = {
    val program = Source.fromFile("input.txt").mkString.trim.split(",").map(_.toLong)

    val computers = (0 until 50).map { address =>
      new IntcodeComputer(program.clone(), mutable.Queue(address.toLong))
    }

    val packetQueues = (0 until 50).map(_ => mutable.Queue[(Long, Long)]())
    var natPacket: Option[(Long, Long)] = None
    var prevNatY: Option[Long] = None

    while (true) {
      var networkIdle = true

      for (i <- computers.indices) {
        val computer = computers(i)
        if (packetQueues(i).nonEmpty) {
          val (x, y) = packetQueues(i).dequeue()
          computer.inputs.enqueue(x, y)
          computer.idle = false
        } else {
          computer.inputs.enqueue(-1)
        }

        computer.run()

        while (computer.outputs.length >= 3) {
          networkIdle = false
          val dest = computer.outputs.dequeue().toInt
          val x = computer.outputs.dequeue()
          val y = computer.outputs.dequeue()

          if (dest == 255) {
            natPacket = Some((x, y))
          } else if (0 <= dest && dest < 50) {
            packetQueues(dest).enqueue((x, y))
          }
        }
      }

      if (computers.indices.forall(i => packetQueues(i).isEmpty && computers(i).idle)) {
        natPacket match {
          case Some((x, y)) =>
            packetQueues(0).enqueue((x, y))
            if (prevNatY.contains(y)) {
              println(y)
              sys.exit(0)
            }
            prevNatY = Some(y)
          case None =>
        }
        networkIdle = false
      }
    }
  }
}
class IntcodeComputer(program: Array[Long], val inputs: mutable.Queue[Long] = mutable.Queue.empty) {
  val memory: mutable.Map[Long, Long] = mutable.Map.empty ++ program.zipWithIndex.map { case (v, i) => i.toLong -> v }
  var ip: Long = 0
  var relativeBase: Long = 0
  val outputs: mutable.Queue[Long] = mutable.Queue.empty
  var halted: Boolean = false
  var idle: Boolean = false

  def getParam(mode: Int, offset: Int): Long = {
    mode match {
      case 0 => memory.getOrElse(memory.getOrElse(ip + offset, 0L), 0L)
      case 1 => memory.getOrElse(ip + offset, 0L)
      case 2 => memory.getOrElse(relativeBase + memory.getOrElse(ip + offset, 0L), 0L)
      case _ => throw new Exception(s"Unknown mode $mode")
    }
  }

  def setParam(mode: Int, offset: Int, value: Long): Unit = {
    mode match {
      case 0 => memory(memory.getOrElse(ip + offset, 0L)) = value
      case 2 => memory(relativeBase + memory.getOrElse(ip + offset, 0L)) = value
      case _ => throw new Exception(s"Unknown mode $mode")
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
        case 99 =>
          halted = true
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
            setParam(modes(0), 1, -1)
            ip += 2
            idle = true
            return
          } else {
            val value = inputs.dequeue()
            setParam(modes(0), 1, value)
            ip += 2
            idle = false
          }
        case 4 =>
          val param1 = getParam(modes(0), 1)
          outputs.enqueue(param1)
          ip += 2
          idle = false
          if (outputs.length == 3) {
            return
          }
        case 5 | 6 =>
          val param1 = getParam(modes(0), 1)
          val param2 = getParam(modes(1), 2)
          ip = if ((opcode == 5 && param1 != 0) || (opcode == 6 && param1 == 0)) param2 else ip + 3
        case 9 =>
          val param1 = getParam(modes(0), 1)
          relativeBase += param1
          ip += 2
        case _ =>
          throw new Exception(s"Unknown opcode $opcode")
      }
    }
  }
}
