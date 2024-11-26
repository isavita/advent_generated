
import java.security.MessageDigest
import scala.io.Source

object ChessPasswordCracker {
  def md5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    val bytes = md.digest(input.getBytes)
    bytes.map("%02x".format(_)).mkString
  }

  def findPassword(doorId: String): String = {
    var password = ""
    var index = 0

    while (password.length < 8) {
      val hash = md5Hash(doorId + index)
      if (hash.startsWith("00000")) {
        password += hash(5)
        println(s"Found character: ${hash(5)} at index $index")
      }
      index += 1
    }

    password
  }

  def main(args: Array[String]): Unit = {
    // Read Door ID from input.txt
    val doorId = Source.fromFile("input.txt").mkString.trim

    val startTime = System.currentTimeMillis()
    val password = findPassword(doorId)
    val endTime = System.currentTimeMillis()

    println(s"Password: $password")
    println(s"Time taken: ${endTime - startTime} ms")
  }
}
