
import scala.collection.mutable
import scala.io.Source

object Main {

  type Registers = Array[Int]
  type Opcode = (Registers, Int, Int, Int) => Unit

  def addr(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a) + registers(b)
  def addi(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a) + b
  def mulr(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a) * registers(b)
  def muli(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a) * b
  def banr(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a) & registers(b)
  def bani(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a) & b
  def borr(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a) | registers(b)
  def bori(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a) | b
  def setr(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = registers(a)
  def seti(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = a
  def gtir(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = if (a > registers(b)) 1 else 0
  def gtri(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = if (registers(a) > b) 1 else 0
  def gtrr(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = if (registers(a) > registers(b)) 1 else 0
  def eqir(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = if (a == registers(b)) 1 else 0
  def eqri(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = if (registers(a) == b) 1 else 0
  def eqrr(registers: Registers, a: Int, b: Int, c: Int): Unit = registers(c) = if (registers(a) == registers(b)) 1 else 0

  val opcodes: Array[Opcode] = Array(addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)

  def testOpcode(sample: (Registers, Array[Int], Registers), opcode: Opcode): Boolean = {
    val registers = sample._1.clone()
    opcode(registers, sample._2(1), sample._2(2), sample._2(3))
    registers.sameElements(sample._3)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray

    val samples = mutable.ArrayBuffer[(Registers, Array[Int], Registers)]()
    val program = mutable.ArrayBuffer[Array[Int]]()

    var i = 0
    while (i < lines.length) {
      if (lines(i).startsWith("Before:")) {
        val before = lines(i).substring(9, lines(i).length - 1).split(", ").map(_.toInt)
        val instruction = lines(i + 1).split(" ").map(_.toInt)
        val after = lines(i + 2).substring(9, lines(i + 2).length - 1).split(", ").map(_.toInt)
        samples += ((before, instruction, after))
        i += 4
      } else if (lines(i).nonEmpty && lines(i)(0).isDigit) {
        program += lines(i).split(" ").map(_.toInt)
        i += 1
      }
      else{
        i+=1
      }
    }
    println(partOne(samples.toArray))
    println(partTwo(samples.toArray, program.toArray))
  }

  def partOne(samples: Array[(Registers, Array[Int], Registers)]): Int = {
    samples.count(sample => opcodes.count(opcode => testOpcode(sample, opcode)) >= 3)
  }

  def partTwo(samples: Array[(Registers, Array[Int], Registers)], program: Array[Array[Int]]): Int = {
    val possibleOpcodes = Array.fill(16)(opcodes.toSet)

    samples.foreach { case (before, instruction, after) =>
      val opcodeNum = instruction(0)
      possibleOpcodes(opcodeNum) = possibleOpcodes(opcodeNum).filter(opcode => testOpcode((before, instruction, after), opcode))
    }

    val opcodeMapping = new Array[Opcode](16)
    val determined = new Array[Boolean](16)

    while (determined.count(_ == true) < 16) {
      for (i <- 0 until 16) {
        if (!determined(i) && possibleOpcodes(i).size == 1) {
          val opcode = possibleOpcodes(i).head
          opcodeMapping(i) = opcode
          determined(i) = true
          for (j <- 0 until 16) {
            if (j != i) {
              possibleOpcodes(j) = possibleOpcodes(j).filter(_ != opcode)
            }
          }
        }
      }
    }

    val registers = Array(0, 0, 0, 0)
    program.foreach { instruction =>
      val opcode = opcodeMapping(instruction(0))
      opcode(registers, instruction(1), instruction(2), instruction(3))
    }
    registers(0)
  }
}
