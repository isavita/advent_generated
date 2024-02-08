
object Day8 extends App {
  import scala.io.Source

  val instructions = Source.fromFile("input.txt").getLines.toList

  val pattern = "(\\w+) (inc|dec) (-?\\d+) if (\\w+) ([<>=!]+) (-?\\d+)".r

  var registers = Map.empty[String, Int]
  var maxValue = Int.MinValue

  def checkCondition(register: String, op: String, value: Int): Boolean = {
    val regValue = registers.getOrElse(register, 0)
    op match {
      case "==" => regValue == value
      case "!=" => regValue != value
      case ">" => regValue > value
      case "<" => regValue < value
      case ">=" => regValue >= value
      case "<=" => regValue <= value
    }
  }

  for (instruction <- instructions) {
    val pattern(reg, incDec, amount, checkReg, op, checkValue) = instruction

    if (checkCondition(checkReg, op, checkValue.toInt)) {
      val value = registers.getOrElse(reg, 0)
      val newValue = if (incDec == "inc") value + amount.toInt else value - amount.toInt
      registers += (reg -> newValue)
      if (newValue > maxValue) maxValue = newValue
    }
  }

  println(registers.values.max)
  println(maxValue)
}
