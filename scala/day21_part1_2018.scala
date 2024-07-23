
import scala.io.Source

object Main extends App {
  val input = Source.fromFile("input.txt").getLines().mkString("\n")
  println(solve(input))

  def solve(input: String): Int = {
    val (instructionPointer, instructions) = parseInput(input)
    val registers = Array.fill(6)(0)
    var ip = 0

    while (ip < instructions.length) {
      registers(instructionPointer) = ip
      if (registers(instructionPointer) == 28) return registers(5)
      val (name, abcValues) = instructions(registers(instructionPointer))
      execute(name, registers, abcValues)
      ip = registers(instructionPointer) + 1
    }
    registers(5)
  }

  def parseInput(input: String): (Int, Array[(String, Array[Int])]) = {
    val lines = input.split("\n")
    val instructionPointer = lines.head.split(" ")(1).toInt
    val instructions = lines.tail.map { line =>
      val parts = line.split(" ")
      (parts(0), Array(parts(1).toInt, parts(2).toInt, parts(3).toInt))
    }
    (instructionPointer, instructions)
  }

  def execute(name: String, registers: Array[Int], abcValues: Array[Int]): Unit = {
    name match {
      case "addr" => registers(abcValues(2)) = registers(abcValues(0)) + registers(abcValues(1))
      case "addi" => registers(abcValues(2)) = registers(abcValues(0)) + abcValues(1)
      case "mulr" => registers(abcValues(2)) = registers(abcValues(0)) * registers(abcValues(1))
      case "muli" => registers(abcValues(2)) = registers(abcValues(0)) * abcValues(1)
      case "banr" => registers(abcValues(2)) = registers(abcValues(0)) & registers(abcValues(1))
      case "bani" => registers(abcValues(2)) = registers(abcValues(0)) & abcValues(1)
      case "borr" => registers(abcValues(2)) = registers(abcValues(0)) | registers(abcValues(1))
      case "bori" => registers(abcValues(2)) = registers(abcValues(0)) | abcValues(1)
      case "setr" => registers(abcValues(2)) = registers(abcValues(0))
      case "seti" => registers(abcValues(2)) = abcValues(0)
      case "gtir" => registers(abcValues(2)) = if (abcValues(0) > registers(abcValues(1))) 1 else 0
      case "gtri" => registers(abcValues(2)) = if (registers(abcValues(0)) > abcValues(1)) 1 else 0
      case "gtrr" => registers(abcValues(2)) = if (registers(abcValues(0)) > registers(abcValues(1))) 1 else 0
      case "eqir" => registers(abcValues(2)) = if (abcValues(0) == registers(abcValues(1))) 1 else 0
      case "eqri" => registers(abcValues(2)) = if (registers(abcValues(0)) == abcValues(1)) 1 else 0
      case "eqrr" => registers(abcValues(2)) = if (registers(abcValues(0)) == registers(abcValues(1))) 1 else 0
    }
  }
}
