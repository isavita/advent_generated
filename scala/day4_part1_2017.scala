
object Day4 {
  def main(args: Array[String]): Unit = {
    val passphrases = scala.io.Source.fromFile("input.txt").getLines().toList
    val validPassphrases = passphrases.filter(isValidPassphrase)
    println(validPassphrases.length)
  }

  def isValidPassphrase(passphrase: String): Boolean = {
    val words = passphrase.split(" ")
    words.distinct.length == words.length
  }
}
