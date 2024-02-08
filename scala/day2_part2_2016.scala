
object Main extends App {
  val instructions = scala.io.Source.fromFile("input.txt").getLines.toList
  val keypad1 = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
  val keypad2 = Array(Array(' ', ' ', '1', ' ', ' '), Array(' ', '2', '3', '4', ' '), Array('5', '6', '7', '8', '9'), Array(' ', 'A', 'B', 'C', ' '), Array(' ', ' ', 'D', ' ', ' '))
  var code1 = ""
  var code2 = ""
  var pos1 = (1, 1)
  var pos2 = (2, 0)
  
  for (inst <- instructions) {
    for (move <- inst) {
      move match {
        case 'U' => if (pos1._1 > 0) pos1 = (pos1._1 - 1, pos1._2)
                    if (pos2._1 > 0 && keypad2(pos2._1 - 1)(pos2._2) != ' ') pos2 = (pos2._1 - 1, pos2._2)
        case 'D' => if (pos1._1 < 2) pos1 = (pos1._1 + 1, pos1._2)
                    if (pos2._1 < 4 && keypad2(pos2._1 + 1)(pos2._2) != ' ') pos2 = (pos2._1 + 1, pos2._2)
        case 'L' => if (pos1._2 > 0) pos1 = (pos1._1, pos1._2 - 1)
                    if (pos2._2 > 0 && keypad2(pos2._1)(pos2._2 - 1) != ' ') pos2 = (pos2._1, pos2._2 - 1)
        case 'R' => if (pos1._2 < 2) pos1 = (pos1._1, pos1._2 + 1)
                    if (pos2._2 < 4 && keypad2(pos2._1)(pos2._2 + 1) != ' ') pos2 = (pos2._1, pos2._2 + 1)
      }
    }
    code1 += keypad1(pos1._1)(pos1._2)
    code2 += keypad2(pos2._1)(pos2._2)
  }
  
  println(code1)
  println(code2)
}
