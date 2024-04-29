import scala.io.Source
import scala.collection.mutable.Map

object Main {
  def main(args: Array[String]): Unit = {
    val instructions = readInstructions("input.txt")
    var registers = Map("a" -> 7, "b" -> 0, "c" -> 0, "d" -> 0)
    executeInstructions(instructions, registers)
    println(registers("a"))
  }

  def readInstructions(filename: String): Array[String] = {
    Source.fromFile(filename).getLines().toArray
  }

  def executeInstructions(instructions: Array[String], registers: Map[String, Int]): Unit = {
    var pc = 0
    while (pc < instructions.length) {
      val fields = instructions(pc).split(" ")
      fields(0) match {
        case "cpy" =>
          val x = getValue(fields(1), registers)
          if (registers.contains(fields(2))) registers(fields(2)) = x
        case "inc" =>
          if (registers.contains(fields(1))) registers(fields(1)) += 1
        case "dec" =>
          if (registers.contains(fields(1))) registers(fields(1)) -= 1
        case "jnz" =>
          val x = getValue(fields(1), registers)
          if (x != 0) pc += getValue(fields(2), registers) - 1
        case "tgl" =>
          val x = getValue(fields(1), registers)
          val tgt = pc + x
          if (tgt >= 0 && tgt < instructions.length) instructions(tgt) = toggleInstruction(instructions(tgt))
      }
      pc += 1
    }
  }

  def getValue(s: String, registers: Map[String, Int]): Int = {
    try s.toInt
    catch { case _: Throwable => registers(s) }
  }

  def toggleInstruction(instr: String): String = {
    val parts = instr.split(" ")
    parts(0) match {
      case "inc" => parts(0) = "dec"
      case "dec" | "tgl" => parts(0) = "inc"
      case "jnz" => parts(0) = "cpy"
      case "cpy" => parts(0) = "jnz"
    }
    parts.mkString(" ")
  }
}