
import scala.io.Source
import java.security.MessageDigest

object Main extends App {
  val salt = Source.fromFile("input.txt").getLines().mkString.trim
  var keys = 0
  var index = 0

  def getMD5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    val bytes = md.digest(input.getBytes)
    bytes.map("%02x".format(_)).mkString
  }

  def findTriplet(hash: String): String = {
    for (i <- 0 until hash.length - 2) {
      if (hash(i) == hash(i + 1) && hash(i) == hash(i + 2)) {
        return hash(i).toString
      }
    }
    ""
  }

  while (keys < 64) {
    val hash = getMD5Hash(salt + index.toString)
    val triplet = findTriplet(hash)
    if (triplet != "") {
      for (i <- 1 to 1000) {
        val nextHash = getMD5Hash(salt + (index + i).toString)
        if (nextHash.contains(triplet * 5)) {
          keys += 1
          if (keys == 64) {
            println(index)
          }
        }
      }
    }
    index += 1
  }
}
