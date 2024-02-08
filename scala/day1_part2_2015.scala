object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString
  val floor = input.map {
    case '(' => 1
    case ')' => -1
  }.sum
  println(floor)

  val firstBasement = input.zipWithIndex.scanLeft(0) {
    case (floor, (char, index)) => floor + (if (char == '(') 1 else -1)
  }.indexWhere(_ == -1)
  println(firstBasement)
}