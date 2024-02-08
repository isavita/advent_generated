
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString
  val captchaLength = input.length
  val sum1 = (0 until captchaLength).map(i => if (input(i) == input((i + 1) % captchaLength)) input(i).asDigit else 0).sum
  val sum2 = (0 until captchaLength).map(i => if (input(i) == input((i + captchaLength / 2) % captchaLength)) input(i).asDigit else 0).sum
  println(sum1)
  println(sum2)
}
