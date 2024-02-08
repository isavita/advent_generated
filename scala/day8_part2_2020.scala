object Main extends App {
  import scala.io.Source

  val instructions = Source.fromFile("input.txt").getLines.toList

  def executeBootCode(instructions: List[String]): (Int, Boolean) = {
    var accumulator = 0
    var visited = Map[Int, Boolean]()
    var currentInstruction = 0

    while (currentInstruction < instructions.length) {
      if (visited.contains(currentInstruction)) {
        return (accumulator, false)
      }

      visited += (currentInstruction -> true)
      val Array(op, arg) = instructions(currentInstruction).split(" ")

      op match {
        case "acc" =>
          accumulator += arg.toInt
          currentInstruction += 1
        case "jmp" =>
          currentInstruction += arg.toInt
        case "nop" =>
          currentInstruction += 1
      }
    }

    (accumulator, true)
  }

  for (i <- instructions.indices) {
    val Array(op, arg) = instructions(i).split(" ")
    if (op == "acc") {
      ()
    } else {
      val modifiedInstructions = instructions.updated(i, if (op == "jmp") s"nop $arg" else s"jmp $arg")
      val (accumulator, terminated) = executeBootCode(modifiedInstructions)
      if (terminated) {
        println(accumulator)
      }
    }
  }
}