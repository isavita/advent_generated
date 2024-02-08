
import java.security.MessageDigest
import java.math.BigInteger
import scala.io.Source

object Main extends App {
  val source = Source.fromFile("input.txt")
  val doorID = source.getLines().mkString.trim
  source.close()

  val password = findPassword(doorID)
  println(password)

  def findPassword(doorID: String): String = {
    var password = Array.fill[Char](8)(' ')
    var filledPositions = 0
    var found = Array.fill[Boolean](8)(false)

    var i = 0
    while (filledPositions < 8) {
      val hash = md5Hash(doorID + i)
      if (hash.startsWith("00000")) {
        val pos = hash.charAt(5)
        if (pos >= '0' && pos <= '7') {
          val posIndex = pos - '0'
          if (!found(posIndex)) {
            found(posIndex) = true
            password(posIndex) = hash.charAt(6)
            filledPositions += 1
          }
        }
      }
      i += 1
    }

    password.mkString
  }

  def md5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    md.update(input.getBytes)
    val digest = md.digest()
    val bigInt = new BigInteger(1, digest)
    String.format("%032x", bigInt)
  }
}
