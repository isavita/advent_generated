
import scala.io.Source

object Duet {
  def main(args: Array[String]): Unit = {
    // Read instructions from input.txt
    val instructions = Source.fromFile("input.txt").getLines().toList

    // Initialize registers and variables
    val registers = scala.collection.mutable.Map[Char, Long]().withDefaultValue(0)
    var lastSoundFrequency: Option[Long] = None
    var instructionPointer = 0

    // Function to get the value of a register or a number
    def getValue(x: String): Long = {
      if (x.forall(_.isDigit) || x.startsWith("-")) x.toLong
      else registers(x.head)
    }

    // Execute instructions
    while (instructionPointer >= 0 && instructionPointer < instructions.length) {
      val instruction = instructions(instructionPointer).split(" ")
      instruction match {
        case Array("snd", x) =>
          lastSoundFrequency = Some(getValue(x))
          instructionPointer += 1
        case Array("set", x, y) =>
          registers(x.head) = getValue(y)
          instructionPointer += 1
        case Array("add", x, y) =>
          registers(x.head) += getValue(y)
          instructionPointer += 1
        case Array("mul", x, y) =>
          registers(x.head) *= getValue(y)
          instructionPointer += 1
        case Array("mod", x, y) =>
          registers(x.head) %= getValue(y)
          instructionPointer += 1
        case Array("rcv", x) =>
          if (getValue(x) != 0) {
            println(s"Recovered frequency: ${lastSoundFrequency.getOrElse(0)}")
            return // Exit after recovering the frequency
          }
          instructionPointer += 1
        case Array("jgz", x, y) =>
          if (getValue(x) > 0) {
            instructionPointer += getValue(y).toInt
          } else {
            instructionPointer += 1
          }
      }
    }
  }
}
