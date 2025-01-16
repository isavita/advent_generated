
import scala.io.Source
import scala.collection.mutable

object Solution {
  def main(args: Array[String]): Unit = {
    val vm = new VM("input.txt")
    val done = new java.util.concurrent.Semaphore(0)

    new Thread(() => vm.run()).start()
    new Thread(() => reader(vm.output, done)).start()

    val instructions = Array(
      "NOT A J",
      "NOT B T",
      "OR T J",
      "NOT C T",
      "OR T J",
      "AND D J",
      "NOT A T",
      "AND A T",
      "OR E T",
      "OR H T",
      "AND T J",
      "RUN"
    )

    instructions.foreach(i => sendString(vm.input, i))
    done.acquire()
  }

  def sendString(ch: mutable.Queue[Int], s: String): Unit = {
    s.foreach(r => ch.enqueue(r.toInt))
    ch.enqueue('\n'.toInt)
  }

  def reader(input: mutable.Queue[Int], done: java.util.concurrent.Semaphore): Unit = {
    while (true) {
      if (input.nonEmpty) {
        val c = input.dequeue()
        if (c > 127) {
          println(c)
          done.release()
          return
        }
      } else {
        Thread.sleep(1)
      }
    }
  }
}

class VM(filename: String) {
  val code: mutable.Map[Int, Int] = mutable.Map.empty
  var ip: Int = 0
  val input: mutable.Queue[Int] = mutable.Queue.empty
  val output: mutable.Queue[Int] = mutable.Queue.empty
  var relativeBase: Int = 0

  load(filename)

  def load(filename: String): Unit = {
    val listStr = Source.fromFile(filename).getLines().mkString.trim.split(",")
    listStr.indices.foreach(i => code(i) = listStr(i).toInt)
    ip = 0
    relativeBase = 0
  }

  def run(): Unit = {
    var arity: Int = 0

    while (true) {
      val cmd = Cmd(code.getOrElse(ip, 0))

      cmd.opCode match {
        case 1 =>
          arity = 3
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(2)) = code.getOrElse(params(0), 0) + code.getOrElse(params(1), 0)

        case 2 =>
          arity = 3
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(2)) = code.getOrElse(params(0), 0) * code.getOrElse(params(1), 0)

        case 3 =>
          arity = 1
          while (input.isEmpty) {
            Thread.sleep(1)
          }
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(0)) = input.dequeue()

        case 4 =>
          arity = 1
          val params = getParamsAddresses(ip, cmd, arity)
          output.enqueue(code.getOrElse(params(0), 0))

        case 5 =>
          arity = 2
          val params = getParamsAddresses(ip, cmd, arity)
          if (code.getOrElse(params(0), 0) != 0) {
            ip = code.getOrElse(params(1), 0)
            
          } else {
            ip += arity + 1
          }

        case 6 =>
          arity = 2
          val params = getParamsAddresses(ip, cmd, arity)
          if (code.getOrElse(params(0), 0) == 0) {
            ip = code.getOrElse(params(1), 0)
            
          } else {
            ip += arity + 1
          }

        case 7 =>
          arity = 3
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(2)) = if (code.getOrElse(params(0), 0) < code.getOrElse(params(1), 0)) 1 else 0

        case 8 =>
          arity = 3
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(2)) = if (code.getOrElse(params(0), 0) == code.getOrElse(params(1), 0)) 1 else 0

        case 9 =>
          arity = 1
          val params = getParamsAddresses(ip, cmd, arity)
          relativeBase += code.getOrElse(params(0), 0)

        case 99 =>
          return

        case _ =>
          throw new RuntimeException(s"not an opcode $cmd")
      }
      if (cmd.opCode != 5 && cmd.opCode != 6)
        ip += arity + 1
    }
  }

  def getParamsAddresses(pos: Int, cmd: Cmd, arity: Int): Array[Int] = {
    val modes = cmd.modes(arity)
    val results = new Array[Int](arity)
    (0 until arity).foreach(i => results(i) = getParamAddress(pos + i + 1, modes(i)))
    results
  }

  def getParamAddress(pos: Int, mode: Mode): Int = {
    mode match {
      case Position => code.getOrElse(pos, 0)
      case Immediate => pos
      case Relative => relativeBase + code.getOrElse(pos, 0)
    }
  }
}

sealed trait Mode
case object Position extends Mode
case object Immediate extends Mode
case object Relative extends Mode

case class Cmd(value: Int) {
  def opCode: Int = value % 100

  def modes(arity: Int): Array[Mode] = {
    val modeSection = value / 100
    val modes = new Array[Mode](arity)
    (0 until arity).foreach(i => modes(i) = (modeSection / Math.pow(10, i).toInt) % 10 match {
      case 0 => Position
      case 1 => Immediate
      case 2 => Relative
    })
    modes
  }
}
