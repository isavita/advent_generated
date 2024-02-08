
import scala.io.Source
import scala.util.matching.Regex

object Main extends App {
  val source = Source.fromFile("input.txt")
  val lines = source.getLines().toList
  source.close()

  var sslCount = 0
  for (line <- lines) {
    if (supportsSSL(line)) {
      sslCount += 1
    }
  }

  println(sslCount)

  def supportsSSL(ip: String): Boolean = {
    val insideBrackets: Regex = """\[[a-z]+\]""".r
    val bracketContents: List[String] = insideBrackets.findAllIn(ip).toList

    var modifiedIp: String = insideBrackets.replaceAllIn(ip, "-")
    for (aba <- findABAs(modifiedIp)) {
      val bab: String = aba(1).toString + aba(0).toString + aba(1).toString
      for (bracketContent <- bracketContents) {
        if (bracketContent.contains(bab)) {
          return true
        }
      }
    }

    false
  }

  def findABAs(s: String): List[String] = {
    var abas: List[String] = List()
    for (i <- 0 until s.length - 2) {
      if (s(i) != s(i + 1) && s(i) == s(i + 2)) {
        abas = abas :+ s.slice(i, i + 3)
      }
    }
    abas
  }
}
