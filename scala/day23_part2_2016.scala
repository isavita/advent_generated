
import scala.io.Source

object Day23 {
  def isRegister(x: String): Boolean = "abcd".contains(x)

  def getValue(x: String, registers: Map[String, Int]): Int =
    if (isRegister(x)) registers(x) else x.toInt

  def executeProgram(instructions: Array[String], registers: Map[String, Int]): Map[String, Int] = {
    var regs = registers
    var i = 0
    while (i < instructions.length) {
      if (i + 5 < instructions.length) {
        val pattern = instructions.slice(i, i + 6)
        if (pattern(0).startsWith("cpy") &&
          pattern(1).startsWith("inc") &&
          pattern(2).startsWith("dec") &&
          pattern(3).startsWith("jnz") &&
          pattern(4).startsWith("dec") &&
          pattern(5).startsWith("jnz")) {

          val Array(_, cpy_x, cpy_y) = pattern(0).split(" ")
          val Array(_, inc_a) = pattern(1).split(" ")
          val Array(_, dec_c) = pattern(2).split(" ")
          val Array(_, jnz_c, jnz_c_offset) = pattern(3).split(" ")
          val Array(_, dec_d) = pattern(4).split(" ")
          val Array(_, jnz_d, jnz_d_offset) = pattern(5).split(" ")

          if (inc_a == "a" && dec_c == cpy_y && jnz_c == cpy_y && jnz_c_offset.toInt == -2 &&
            dec_d == "d" && jnz_d == "d" && jnz_d_offset.toInt == -5) {
            regs = regs.updated("a", regs("a") + regs(cpy_x) * regs("d"))
            regs = regs.updated(cpy_y, 0)
            regs = regs.updated("d", 0)
            i += 6
            
          }
        }
      }
      if(i < instructions.length){
        val parts = instructions(i).split(" ")
        val cmd = parts(0)

        cmd match {
          case "tgl" =>
            val x = getValue(parts(1), regs)
            val targetIdx = i + x
            if (targetIdx >= 0 && targetIdx < instructions.length) {
              val targetParts = instructions(targetIdx).split(" ")
              if (targetParts.length == 2) {
                targetParts(0) = if (targetParts(0) == "inc") "dec" else "inc"
              } else if (targetParts.length == 3) {
                targetParts(0) = if (targetParts(0) == "jnz") "cpy" else "jnz"
              }
              instructions(targetIdx) = targetParts.mkString(" ")
            }
            i += 1

          case "cpy" =>
            val x = parts(1)
            val y = parts(2)
            if (isRegister(y)) {
              regs = regs.updated(y, getValue(x, regs))
            }
            i += 1
          case "inc" =>
            val x = parts(1)
            if (isRegister(x)) {
              regs = regs.updated(x, regs(x) + 1)
            }
            i += 1
          case "dec" =>
            val x = parts(1)
            if (isRegister(x)) {
              regs = regs.updated(x, regs(x) - 1)
            }
            i += 1
          case "jnz" =>
            val x = parts(1)
            val y = parts(2)
            if (getValue(x, regs) != 0) {
              i += getValue(y, regs)
            } else {
              i += 1
            }
          case _ =>
            i += 1
        }
      }
    }
    regs
  }

  def main(args: Array[String]): Unit = {
    val instructions = Source.fromFile("input.txt").getLines().toArray
    val registers = Map("a" -> 12, "b" -> 0, "c" -> 0, "d" -> 0)
    val finalRegisters = executeProgram(instructions, registers)
    println(finalRegisters("a"))
  }
}
