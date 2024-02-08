import scala.io.Source
import java.security.MessageDigest

object AdventCoins {
  def main(args: Array[String]): Unit = {
    val secretKey = Source.fromFile("input.txt").getLines().next()
    val md = MessageDigest.getInstance("MD5")

    def md5(s: String): String = {
      md.digest(s.getBytes).map("%02x".format(_)).mkString
    }

    def mineAdventCoin(key: String): Int = {
      Stream.from(1).find(i => md5(key + i).startsWith("00000")).get
    }

    val result = mineAdventCoin(secretKey)
    println(result)
  }
}