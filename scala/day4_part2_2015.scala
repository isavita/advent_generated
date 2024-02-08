object Solution extends App {
  import java.security.MessageDigest
  import scala.io.Source

  val secretKey = Source.fromFile("input.txt").getLines().mkString.trim
  var number = 0
  val md5 = MessageDigest.getInstance("MD5")

  while (true) {
    val hash = md5.digest((secretKey + number.toString).getBytes)
    val hashString = hash.map("%02x".format(_)).mkString

    if (hashString.startsWith("000000")) {
      println(number)
      System.exit(0)
    }
    number += 1
  }
}