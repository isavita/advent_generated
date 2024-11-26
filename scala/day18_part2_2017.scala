
import scala.io.Source
import scala.collection.mutable

object Day18 {
  def part1(instructions: List[String]): Long = {
    val registers = mutable.Map[Char, Long]().withDefaultValue(0L)
    var lastSound = 0L
    var pc = 0

    def getValue(x: String): Long = {
      if (x.head.isLetter) registers(x.head)
      else x.toLong
    }

    while (pc >= 0 && pc < instructions.length) {
      val parts = instructions(pc).split(" ")
      parts(0) match {
        case "snd" => lastSound = getValue(parts(1))
        case "set" => registers(parts(1).head) = getValue(parts(2))
        case "add" => registers(parts(1).head) += getValue(parts(2))
        case "mul" => registers(parts(1).head) *= getValue(parts(2))
        case "mod" => registers(parts(1).head) %= getValue(parts(2))
        case "rcv" => if (getValue(parts(1)) != 0) return lastSound
        case "jgz" => if (getValue(parts(1)) > 0) pc += getValue(parts(2)).toInt - 1
      }
      pc += 1
    }
    lastSound
  }

  def part2(instructions: List[String]): Int = {
    val program0 = new Program(0, instructions)
    val program1 = new Program(1, instructions)

    program0.otherProgram = program1
    program1.otherProgram = program0

    while (!program0.isBlocked || !program1.isBlocked) {
      program0.execute()
      program1.execute()
    }

    program1.sendCount
  }

  class Program(val id: Int, instructions: List[String]) {
    val registers = mutable.Map[Char, Long]('p' -> id.toLong).withDefaultValue(0L)
    var pc = 0
    var sendCount = 0
    var isBlocked = false
    var otherProgram: Program = _
    val sendQueue = mutable.Queue[Long]()
    val receiveQueue = mutable.Queue[Long]()

    def getValue(x: String): Long = {
      if (x.head.isLetter) registers(x.head)
      else x.toLong
    }

    def execute(): Unit = {
      if (pc < 0 || pc >= instructions.length) {
        isBlocked = true
        return
      }

      val parts = instructions(pc).split(" ")
      parts(0) match {
        case "snd" =>
          otherProgram.receiveQueue.enqueue(getValue(parts(1)))
          sendCount += 1
          isBlocked = false
        case "set" => registers(parts(1).head) = getValue(parts(2))
        case "add" => registers(parts(1).head) += getValue(parts(2))
        case "mul" => registers(parts(1).head) *= getValue(parts(2))
        case "mod" => registers(parts(1).head) %= getValue(parts(2))
        case "rcv" =>
          if (receiveQueue.isEmpty) {
            isBlocked = true
            return
          }
          registers(parts(1).head) = receiveQueue.dequeue()
          isBlocked = false
        case "jgz" => if (getValue(parts(1)) > 0) pc += getValue(parts(2)).toInt - 1
      }
      pc += 1
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }
}
