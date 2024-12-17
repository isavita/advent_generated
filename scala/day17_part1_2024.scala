
import scala.io.Source
import scala.util.Using

object Solution {
  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("input.txt")) { source =>
      val lines = source.getLines().toList
      var A = 0
      var B = 0
      var C = 0
      var program = List[Int]()

      lines.foreach { line =>
        val trimmedLine = line.trim
        if (trimmedLine.startsWith("Register A:")) {
          A = trimmedLine.split(":").last.trim.toInt
        } else if (trimmedLine.startsWith("Register B:")) {
          B = trimmedLine.split(":").last.trim.toInt
        } else if (trimmedLine.startsWith("Register C:")) {
          C = trimmedLine.split(":").last.trim.toInt
        } else if (trimmedLine.startsWith("Program:")) {
          program = trimmedLine.split(":").last.trim.split(",").map(_.trim.toInt).toList
        }
      }

      def getComboVal(op: Int): Int = op match {
        case op if op <= 3 => op
        case 4 => A
        case 5 => B
        case 6 => C
        case _ => throw new IllegalArgumentException("invalid combo operand")
      }

      var outputVals = List[String]()
      var ip = 0
      while (ip < program.length) {
        val opcode = program(ip)
        if (ip + 1 >= program.length) {
          ip = program.length
        } else {
          val operand = program(ip + 1)
          opcode match {
            case 0 =>
              val den = getComboVal(operand)
              A = if (den == 0) 0 else A / (1 << den)
              ip += 2
            case 1 =>
              B = B ^ operand
              ip += 2
            case 2 =>
              B = getComboVal(operand) % 8
              ip += 2
            case 3 =>
              if (A != 0) ip = operand else ip += 2
            case 4 =>
              B = B ^ C
              ip += 2
            case 5 =>
              outputVals = outputVals :+ (getComboVal(operand) % 8).toString
              ip += 2
            case 6 =>
              val den = getComboVal(operand)
              B = A / (1 << den)
              ip += 2
            case 7 =>
              val den = getComboVal(operand)
              C = A / (1 << den)
              ip += 2
            case _ =>
              ip = program.length
          }
        }
      }
      println(outputVals.mkString(","))
    }
  }
}
