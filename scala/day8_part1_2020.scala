
object Day8 extends App {
  val instructions = scala.io.Source.fromFile("input.txt").getLines.toList
  var acc = 0
  var index = 0
  var visited = Set[Int]()

  while (!visited.contains(index)) {
    visited += index
    val Array(op, arg) = instructions(index).split(" ")
    op match {
      case "acc" => {
        acc += arg.toInt
        index += 1
      }
      case "jmp" => index += arg.toInt
      case "nop" => index += 1
    }
  }

  println(acc)
}
