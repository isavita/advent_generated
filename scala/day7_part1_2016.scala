object Solution extends App {
  import scala.io.Source
  import scala.util.matching.Regex

  val source = Source.fromFile("input.txt")
  val lines = source.getLines().toList
  source.close()

  val tlsCount = lines.count(supportsTLS)

  println(tlsCount)

  def supportsTLS(ip: String): Boolean = {
    val insideBrackets = """\[[a-z]+\]""".r
    val bracketContents = insideBrackets.findAllIn(ip).toList

    bracketContents.exists(containsABBA) match {
      case true => false
      case false =>
        val ipWithoutBrackets = insideBrackets.replaceAllIn(ip, "-")
        containsABBA(ipWithoutBrackets)
    }
  }

  def containsABBA(s: String): Boolean = {
    s.sliding(4).exists { chunk =>
      chunk(0) != chunk(1) && chunk(0) == chunk(3) && chunk(1) == chunk(2)
    }
  }
}