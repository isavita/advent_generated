import scala.io.Source

object Solution {
  def validatePassword(policy: String, password: String): Boolean = {
    val Array(min, max, char) = policy.split("-|\\s+")
    (password(min.toInt - 1) == char.head) != (password(max.toInt - 1) == char.head)
  }

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val validCount = Source.fromFile(filename).getLines().count { line =>
      val Array(policy, password) = line.split(":").map(_.trim)
      validatePassword(policy, password)
    }
    
    println(validCount)
  }
}