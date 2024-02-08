
import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val currentPassword = readInput("input.txt")
    val newPassword = findNextPassword(currentPassword)
    println(newPassword)
  }

  def readInput(filename: String): String = {
    val source = Source.fromFile(filename)
    val input = source.getLines().next()
    source.close()
    input
  }

  def findNextPassword(password: String): String = {
    var newPass = password
    while (!isValidPassword(newPass)) {
      newPass = incrementPassword(newPass)
    }
    newPass
  }

  def incrementPassword(password: String): String = {
    var charArr = password.toCharArray()
    for (i <- charArr.length - 1 to 0 by -1) {
      charArr(i) = (charArr(i) + 1).toChar
      if (charArr(i) > 'z') charArr(i) = 'a'
      else return charArr.mkString
    }
    charArr.mkString
  }

  def isValidPassword(password: String): Boolean = {
    hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password)
  }

  def hasStraight(password: String): Boolean = {
    for (i <- 0 until password.length - 2) {
      if (password(i) + 1 == password(i + 1) && password(i) + 2 == password(i + 2)) return true
    }
    false
  }

  def containsInvalidLetters(password: String): Boolean = {
    password.contains('i') || password.contains('o') || password.contains('l')
  }

  def hasTwoPairs(password: String): Boolean = {
    var count = 0
    var i = 0
    while (i < password.length - 1) {
      if (password(i) == password(i + 1)) {
        count += 1
        i += 2
      } else {
        i += 1
      }
    }
    count >= 2
  }
}
