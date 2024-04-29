import scala.io.Source

object Day19 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val ipRegister = lines.head match {
      case s"#ip $register" => register.toInt
      case _ => throw new Exception("Invalid input")
    }
    val instructions = lines.tail.map { line =>
      val parts = line.split(" ")
      (parts(0), parts(1).toInt, parts(2).toInt, parts(3).toInt)
    }
    var ip = 0
    var registers = Array.fill(6)(0)
    registers(ipRegister) = ip
    while (ip >= 0 && ip < instructions.length) {
      val (opcode, a, b, c) = instructions(ip)
      opcode match {
        case "addr" => registers(c) = registers(a) + registers(b)
        case "addi" => registers(c) = registers(a) + b
        case "mulr" => registers(c) = registers(a) * registers(b)
        case "muli" => registers(c) = registers(a) * b
        case "banr" => registers(c) = registers(a) & registers(b)
        case "bani" => registers(c) = registers(a) & b
        case "borr" => registers(c) = registers(a) | registers(b)
        case "bori" => registers(c) = registers(a) | b
        case "setr" => registers(c) = registers(a)
        case "seti" => registers(c) = a
        case "gtir" => registers(c) = if (a > registers(b)) 1 else 0
        case "gtri" => registers(c) = if (registers(a) > b) 1 else 0
        case "gtrr" => registers(c) = if (registers(a) > registers(b)) 1 else 0
        case "eqir" => registers(c) = if (a == registers(b)) 1 else 0
        case "eqri" => registers(c) = if (registers(a) == b) 1 else 0
        case "eqrr" => registers(c) = if (registers(a) == registers(b)) 1 else 0
      }
      ip = registers(ipRegister)
      ip += 1
      registers(ipRegister) = ip
    }
    println(registers(0))
  }
}