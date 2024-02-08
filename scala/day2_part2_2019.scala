object Day2 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.next.split(",").map(_.toInt).toList

  def runProgram(noun: Int, verb: Int): Int = {
    var memory = input.updated(1, noun).updated(2, verb)
    var index = 0

    while (memory(index) != 99) {
      val op = memory(index)
      val a = memory(memory(index + 1))
      val b = memory(memory(index + 2))
      val dest = memory(index + 3)

      memory = op match {
        case 1 => memory.updated(dest, a + b)
        case 2 => memory.updated(dest, a * b)
      }

      index += 4
    }

    memory(0)
  }

  val target = 19690720

  val result = (for {
    noun <- 0 to 99
    verb <- 0 to 99
    if runProgram(noun, verb) == target
  } yield 100 * noun + verb).head

  println(result)
}