
import scala.io.Source

object Main extends App {
  val input = Source.fromFile("input.txt").getLines().mkString("\n")
  println(solve(input))

  def solve(input: String): Int = {
    val (instructionPointer, instructions) = parseInput(input)
    val registers = Array.fill(6)(0)
    var lastReg5 = 0
    val comparedRegister5s = scala.collection.mutable.HashSet[Int]()

    while (true) {
      if (registers(instructionPointer) >= instructions.length) return lastReg5
      val instIndex = registers(instructionPointer)
      val (name, abcValues) = instructions(instIndex)

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

      registers(instructionPointer) += 1
      if (registers(instructionPointer) == 28) {
        val reg5 = registers(5)
        if (comparedRegister5s.contains(reg5)) return lastReg5
        comparedRegister5s.add(reg5)
        lastReg5 = reg5
      }
    }
    lastReg5
  }

  def parseInput(input: String): (Int, Array[(String, Array[Int])]) = {
    val lines = input.split("\n")
    val instructionPointer = lines(0).split(" ")(1).toInt
    val instructions = lines.tail.map { line =>
      val parts = line.split(" ")
      (parts(0), Array(parts(1).toInt, parts(2).toInt, parts(3).toInt))
    }
    (instructionPointer, instructions)
  }
}
