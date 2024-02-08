
import scala.io.Source

object Main extends App {
  def sortString(w: String): String = w.sorted

  val passphrases = Source.fromFile("input.txt").getLines.toList
  var validCount = 0

  passphrases.foreach { passphrase =>
    val words = passphrase.split("\\s+")
    var wordSet = scala.collection.mutable.Set[String]()

    var valid = true
    words.foreach { word =>
      val sortedWord = sortString(word)
      if (wordSet.contains(sortedWord)) {
        valid = false
      }
      wordSet += sortedWord
    }

    if (valid) {
      validCount += 1
    }
  }

  println(validCount)
}
