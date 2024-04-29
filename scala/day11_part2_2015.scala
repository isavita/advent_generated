object Day11 {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").mkString
    val nextPasswordString = nextPassword(input)
    println(nextPasswordString)
    val nextNextPasswordString = nextPassword(nextPasswordString)
    println(nextNextPasswordString)
  }

  def nextPassword(password: String): String = {
    var newPassword = increment(password)
    while (!isValid(newPassword)) {
      newPassword = increment(newPassword)
    }
    newPassword
  }

  def increment(password: String): String = {
    var chars = password.toCharArray
    var i = chars.length - 1
    while (i >= 0) {
      if (chars(i) == 'z') {
        chars(i) = 'a'
        i -= 1
      } else {
        chars(i) = (chars(i) + 1).toChar
        return new String(chars)
      }
    }
    new String(chars)
  }

  def isValid(password: String): Boolean = {
    hasStraight(password) && !hasForbiddenLetters(password) && hasTwoPairs(password)
  }

  def hasStraight(password: String): Boolean = {
    password.sliding(3).exists(window => window(0) + 1 == window(1) && window(1) + 1 == window(2))
  }

  def hasForbiddenLetters(password: String): Boolean = {
    password.contains("i") || password.contains("o") || password.contains("l")
  }

  def hasTwoPairs(password: String): Boolean = {
    val pairs = password.sliding(2).filter(window => window(0) == window(1)).toSet
    pairs.size >= 2
  }
}