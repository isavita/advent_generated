
object Main extends App {
  import scala.io.Source

  val input = Source.fromFile("input.txt").getLines.toList

  var registers = Map[String, Int]().withDefaultValue(0)
  var maxRegisterValue = 0

  def evaluateCondition(register: String, operator: String, value: Int): Boolean = {
    operator match {
      case ">" => registers(register) > value
      case "<" => registers(register) < value
      case ">=" => registers(register) >= value
      case "<=" => registers(register) <= value
      case "==" => registers(register) == value
      case "!=" => registers(register) != value
    }
  }

  input.foreach { line =>
    val Array(regToModify, incDec, amount, _, condReg, condOp, condValue) = line.split(" ")
    if (evaluateCondition(condReg, condOp, condValue.toInt)) {
      val value = amount.toInt * (if (incDec == "inc") 1 else -1)
      registers += (regToModify -> (registers(regToModify) + value))
      if (registers(regToModify) > maxRegisterValue) maxRegisterValue = registers(regToModify)
    }
  }

  println(registers.values.max)
}
