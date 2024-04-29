import scala.io.Source
import scala.util.Try

object Main {
  type Operation = (Array[Int], Int, Int) => Int

  val instructions: Map[String, Operation] = Map(
    "addr" -> ((r, a, b) => r(a) + r(b)),
    "addi" -> ((r, a, b) => r(a) + b),
    "mulr" -> ((r, a, b) => r(a) * r(b)),
    "muli" -> ((r, a, b) => r(a) * b),
    "banr" -> ((r, a, b) => r(a) & r(b)),
    "bani" -> ((r, a, b) => r(a) & b),
    "borr" -> ((r, a, b) => r(a) | r(b)),
    "bori" -> ((r, a, b) => r(a) | b),
    "setr" -> ((r, a, b) => r(a)),
    "seti" -> ((r, a, b) => a),
    "gtir" -> ((r, a, b) => if (a > r(b)) 1 else 0),
    "gtri" -> ((r, a, b) => if (r(a) > b) 1 else 0),
    "gtrr" -> ((r, a, b) => if (r(a) > r(b)) 1 else 0),
    "eqir" -> ((r, a, b) => if (a == r(b)) 1 else 0),
    "eqri" -> ((r, a, b) => if (r(a) == b) 1 else 0),
    "eqrr" -> ((r, a, b) => if (r(a) == r(b)) 1 else 0)
  )

  def loadProgram(lines: Array[String]): (Int, Array[Array[Int] => Unit]) = {
    var ipRegister = 0
    var program: Array[Array[Int] => Unit] = Array()

    for (line <- lines) {
      if (line.startsWith("#ip")) {
        ipRegister = line.split(" ")(1).toInt
      } else {
        val parts = line.split(" ")
        val op = instructions(parts(0))
        val nums = parts.tail.map(_.toInt)
        program = program :+ ((r: Array[Int]) => r(nums(2)) = op(r, nums(0), nums(1)))
      }
    }
    (ipRegister, program)
  }

  def runProgram(ipRegister: Int, program: Array[Array[Int] => Unit], registers: Array[Int], maxCycles: Int): Array[Int] = {
    var ip = 0
    var cycles = 0
    while (ip >= 0 && ip < program.length) {
      registers(ipRegister) = ip
      program(ip)(registers)
      ip = registers(ipRegister) + 1
      cycles += 1
      if (maxCycles > 0 && cycles >= maxCycles) {
        return registers
      }
    }
    registers
  }

  def max(slice: Array[Int]): Int = slice.max

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().filter(_.nonEmpty).toArray
    val (ipRegister, program) = loadProgram(lines)

    val registers = new Array[Int](6)
    registers(0) = 1
    val result = runProgram(ipRegister, program, registers, 1000)
    val n = max(result)
    var total = 0
    for (i <- 1 to n) {
      if (n % i == 0) {
        total += i
      }
    }
    println(total)
  }
}