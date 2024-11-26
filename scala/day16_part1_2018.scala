
import scala.io.Source
import scala.util.matching.Regex

object Solution extends App {
  val source = Source.fromFile("input.txt")
  val lines = source.getLines().toList

  val opcodes = List(
    Opcode("addr", '+', 'r', 'r'),
    Opcode("addi", '+', 'r', 'v'),
    Opcode("mulr", '*', 'r', 'r'),
    Opcode("muli", '*', 'r', 'v'),
    Opcode("banr", '&', 'r', 'r'),
    Opcode("bani", '&', 'r', 'v'),
    Opcode("borr", '|', 'r', 'r'),
    Opcode("bori", '|', 'r', 'v'),
    Opcode("setr", 'a', 'r', 'r'),
    Opcode("seti", 'a', 'v', 'r'),
    Opcode("gtir", '>', 'v', 'r'),
    Opcode("gtri", '>', 'r', 'v'),
    Opcode("gtrr", '>', 'r', 'r'),
    Opcode("eqir", '=', 'v', 'r'),
    Opcode("eqri", '=', 'r', 'v'),
    Opcode("eqrr", '=', 'r', 'r')
  )

  var sum = 0
  var i = 0
  while (i < lines.size) {
    if (lines(i).startsWith("Before")) {
      val before = extractRegisters(lines(i))
      val instruction = extractInstruction(lines(i+1))
      val after = extractRegisters(lines(i+2))
      val matchingOpcodes = opcodes.filter(opcode => matches(after, runOpcode(opcode, before, instruction)))
      if (matchingOpcodes.size >= 3) sum += 1
      i += 4
    } else {
      i += 1
    }
  }
  println(sum)
  source.close()
}


case class Opcode(name: String, action: Char, a: Char, b: Char)

def extractRegisters(line: String): Array[Int] = {
  val r = """\[(\d+), (\d+), (\d+), (\d+)\]""".r
  r.findFirstMatchIn(line).get.subgroups.map(_.toInt).toArray
}

def extractInstruction(line: String): Array[Int] = {
  line.split(" ").map(_.toInt).toArray
}

def runOpcode(opcode: Opcode, registers: Array[Int], instruction: Array[Int]): Array[Int] = {
  val newRegisters = registers.clone()
  val a = if (opcode.a == 'r') registers(instruction(1)) else instruction(1)
  val b = if (opcode.b == 'r') registers(instruction(2)) else instruction(2)
  opcode.action match {
    case '+' => newRegisters(instruction(3)) = a + b
    case '*' => newRegisters(instruction(3)) = a * b
    case '&' => newRegisters(instruction(3)) = a & b
    case '|' => newRegisters(instruction(3)) = a | b
    case 'a' => newRegisters(instruction(3)) = a
    case '>' => newRegisters(instruction(3)) = if (a > b) 1 else 0
    case '=' => newRegisters(instruction(3)) = if (a == b) 1 else 0
  }
  newRegisters
}

def matches(r1: Array[Int], r2: Array[Int]): Boolean = r1.sameElements(r2)
