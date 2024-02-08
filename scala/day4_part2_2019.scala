
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList.head.split("-").map(_.toInt)
  val range = input(0) to input(1)

  def isValidPasswordPart1(password: Int): Boolean = {
    val digits = password.toString.map(_.asDigit)
    digits.sorted == digits && digits.sliding(2).exists(pair => pair(0) == pair(1))
  }

  def isValidPasswordPart2(password: Int): Boolean = {
    val digits = password.toString.map(_.asDigit)
    digits.sorted == digits && digits.groupBy(identity).values.exists(_.length == 2)
  }

  val validPasswordsPart1 = range.count(isValidPasswordPart1)
  val validPasswordsPart2 = range.count(isValidPasswordPart2)

  println(validPasswordsPart1)
  println(validPasswordsPart2)
}
