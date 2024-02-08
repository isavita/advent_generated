object Day1 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString
  val result = input.foldLeft(0)((acc, char) => if (char == '(') acc + 1 else acc - 1)
  println(result)
}