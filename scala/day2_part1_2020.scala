
object PasswordPhilosophy extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  val validPasswords = input.count { line =>
    val Array(policy, password) = line.split(": ")
    val Array(range, char) = policy.split(" ")
    val Array(min, max) = range.split("-").map(_.toInt)

    val count = password.count(_ == char.charAt(0))
    count >= min && count <= max
  }

  println(validPasswords)
}
