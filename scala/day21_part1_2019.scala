
import scala.io.Source
import scala.collection.mutable

object VM {
  def main(args: Array[String]): Unit = {
    val vm = new VM("input.txt")
    val instructions = Seq(
      "NOT A J",
      "NOT B T",
      "OR T J",
      "NOT C T",
      "OR T J",
      "AND D J",
      "WALK"
    )
    instructions.foreach(vm.sendString)
    vm.run()
    vm.output.find(_ > 127).foreach(println)
  }
}
class VM(filename: String) {
  val code: mutable.Map[Long, Long] = mutable.Map()
  var ip: Long = 0
  val input: mutable.Queue[Long] = mutable.Queue()
  val output: mutable.Queue[Long] = mutable.Queue()
  var relativeBase: Long = 0
  load()

  private def load(): Unit = {
    Source.fromFile(filename).getLines().next().split(',').zipWithIndex.foreach {
      case (value, index) => code(index) = value.toLong
    }
  }

  def sendString(s: String): Unit = {
    s.foreach(c => input.enqueue(c.toLong))
    input.enqueue('\n'.toLong)
  }

  def run(): Unit = {
    while (true) {
      val cmd = code.getOrElse(ip, 0L)
      val opcode = cmd % 100
      val modes = (2 to 4).map(i => (cmd / math.pow(10, i).toInt) % 10)

      def getParam(index: Int): Long = {
        val mode = modes(index - 1)
        mode match {
          case 0 => code.getOrElse(code.getOrElse(ip + index, 0L), 0L)
          case 1 => code.getOrElse(ip + index, 0L)
          case 2 => code.getOrElse(relativeBase + code.getOrElse(ip + index, 0L), 0L)
        }
      }

      def getAddress(index: Int): Long = {
        val mode = modes(index - 1)
        mode match {
          case 0 => code.getOrElse(ip + index, 0L)
          case 2 => relativeBase + code.getOrElse(ip + index, 0L)
        }
      }

      opcode match {
        case 1 =>
          code(getAddress(3)) = getParam(1) + getParam(2)
          ip += 4
        case 2 =>
          code(getAddress(3)) = getParam(1) * getParam(2)
          ip += 4
        case 3 =>
          code(getAddress(1)) = input.dequeue()
          ip += 2
        case 4 =>
          output.enqueue(getParam(1))
          ip += 2
        case 5 =>
          ip = if (getParam(1) != 0) getParam(2) else ip + 3
        case 6 =>
          ip = if (getParam(1) == 0) getParam(2) else ip + 3
        case 7 =>
          code(getAddress(3)) = if (getParam(1) < getParam(2)) 1 else 0
          ip += 4
        case 8 =>
          code(getAddress(3)) = if (getParam(1) == getParam(2)) 1 else 0
          ip += 4
        case 9 =>
          relativeBase += getParam(1)
          ip += 2
        case 99 =>
          return
        case _ =>
          throw new Exception(s"Unknown opcode $opcode")
      }
    }
  }
}
