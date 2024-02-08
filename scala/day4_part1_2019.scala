
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next().split("-").map(_.toInt)
  val range = input(0) to input(1)

  def isValidPassword(password: Int): Boolean = {
    val passwordStr = password.toString
    val hasAdjacentDigits = passwordStr.sliding(2).exists(pair => pair(0) == pair(1))
    val neverDecrease = passwordStr.sliding(2).forall(pair => pair(0) <= pair(1))
    hasAdjacentDigits && neverDecrease
  }

  val validPasswords = range.filter(isValidPassword)
  println(validPasswords.length)
}
