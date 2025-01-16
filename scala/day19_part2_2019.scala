
import scala.io.Source
import scala.collection.mutable

object Solution {
  def main(args: Array[String]): Unit = {
    var y = 20
    var x = 0

    while (true) {
      if (!beam(x, y)) {
        x += 1
      } else if (!beam(x + 99, y)) {
        y += 1
      } else if (!beam(x, y + 99)) {
        x += 1
      } else {
        println(x * 10000 + y)
        return
      }
    }
  }

  def beam(x: Int, y: Int): Boolean = {
    val vm = new VM("input.txt")
    vm.run(x, y) == 1
  }
}

class VM(filename: String) {
  val code: mutable.Map[Int, Long] = mutable.Map.empty
  var ip: Int = 0
  var relativeBase: Int = 0

  load(filename)

  def load(filename: String): Unit = {
    val lines = Source.fromFile(filename).getLines.mkString
    val listStr = lines.trim.split(",")
    for (i <- listStr.indices) {
      code(i) = listStr(i).toLong
    }
    ip = 0
    relativeBase = 0
  }

  def run(inputX: Int, inputY: Int): Int = {
    var input = List(inputX, inputY)
    var output = 0

    while (true) {
      val cmd = Cmd(code(ip).toInt)
      var arity = 0

      cmd.opCode match {
        case 1 =>
          arity = 3
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(2)) = code.getOrElse(params(0), 0L) + code.getOrElse(params(1), 0L)
        case 2 =>
          arity = 3
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(2)) = code.getOrElse(params(0), 0L) * code.getOrElse(params(1), 0L)
        case 3 =>
          arity = 1
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(0)) = input.head
          input = input.tail
        case 4 =>
          arity = 1
          val params = getParamsAddresses(ip, cmd, arity)
          output = code.getOrElse(params(0), 0L).toInt
        case 5 =>
          arity = 2
          val params = getParamsAddresses(ip, cmd, arity)
          if (code.getOrElse(params(0), 0L) != 0) {
            ip = code.getOrElse(params(1), 0L).toInt
            arity = -1
          }
        case 6 =>
          arity = 2
          val params = getParamsAddresses(ip, cmd, arity)
          if (code.getOrElse(params(0), 0L) == 0) {
            ip = code.getOrElse(params(1), 0L).toInt
            arity = -1
          }
        case 7 =>
          arity = 3
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(2)) = if (code.getOrElse(params(0), 0L) < code.getOrElse(params(1), 0L)) 1 else 0
        case 8 =>
          arity = 3
          val params = getParamsAddresses(ip, cmd, arity)
          code(params(2)) = if (code.getOrElse(params(0), 0L) == code.getOrElse(params(1), 0L)) 1 else 0
        case 9 =>
          arity = 1
          val params = getParamsAddresses(ip, cmd, arity)
          relativeBase += code.getOrElse(params(0), 0L).toInt
        case 99 =>
          return output
        case _ =>
          throw new RuntimeException(s"not an opcode $cmd")
      }

      if (arity >= 0) {
        ip += arity + 1
      }
    }
    output
  }

  def getParamsAddresses(pos: Int, cmd: Cmd, arity: Int): List[Int] = {
    val modes = cmd.modes(arity)
    (0 until arity).map(i => getParamAddress(pos + i + 1, modes(i))).toList
  }

  def getParamAddress(pos: Int, mode: Mode): Int = {
    mode match {
      case Position  => code.getOrElse(pos, 0L).toInt
      case Immediate => pos
      case Relative  => relativeBase + code.getOrElse(pos, 0L).toInt
    }
  }
}

sealed trait Mode
case object Position extends Mode
case object Immediate extends Mode
case object Relative extends Mode

case class Cmd(value: Int) {
  def opCode: Int = value % 100

  def modes(arity: Int): List[Mode] = {
    val modeSection = value / 100
    (0 until arity).map(i => (modeSection / Math.pow(10, i).toInt) % 10 match {
      case 0 => Position
      case 1 => Immediate
      case 2 => Relative
    }).toList
  }
}
