
import scala.io.Source
import scala.util.matching.Regex

object SevenSegmentSearch extends App {

  val input = Source.fromFile("input.txt").getLines().mkString("\n")

  val ans = jumbledSevenSegment(input)
  println(ans)


  def jumbledSevenSegment(input: String): Int = {
    val lines = input.split("\n")
    var ans = 0

    for (line <- lines) {
      val parts = new Regex("""([a-g]+)""").findAllIn(line).toArray
      if (parts.length != 14) throw new IllegalArgumentException("Invalid input line")

      val fourteen = parts.map(alphabetizeString)
      val indexToCharacters = Array.fill(10)("")

      val workingSet = fourteen.slice(0, 10)
      val killIndices = workingSet.indices.filter(i => workingSet(i).length match {
        case 2 => indexToCharacters(1) = workingSet(i); true
        case 4 => indexToCharacters(4) = workingSet(i); true
        case 3 => indexToCharacters(7) = workingSet(i); true
        case 7 => indexToCharacters(8) = workingSet(i); true
        case _ => false
      })

      val filteredWorkingSet = workingSet.zipWithIndex.filterNot(x => killIndices.contains(x._2)).map(_._1)

      val zeroThreeOrNine = filteredWorkingSet.filter(checkStringOverlap(_, indexToCharacters(1)))
      if (zeroThreeOrNine.length != 3) throw new IllegalArgumentException("Invalid number of 0, 3, 9")

      indexToCharacters(3) = zeroThreeOrNine.find(_.length == 5).get
      val remaining09 = zeroThreeOrNine.filterNot(_ == indexToCharacters(3))
      indexToCharacters(9) = remaining09.find(checkStringOverlap(_, indexToCharacters(4))).get
      indexToCharacters(0) = remaining09.filterNot(_ == indexToCharacters(9)).head

      val remaining = filteredWorkingSet.filterNot(zeroThreeOrNine.contains(_))
      if (remaining.length != 3) throw new IllegalArgumentException("Invalid number of remaining digits")

      indexToCharacters(6) = remaining.find(_.length == 6).get
      val remaining25 = remaining.filterNot(_ == indexToCharacters(6))
      indexToCharacters(5) = remaining25.find(checkStringOverlap(indexToCharacters(9), _)).get
      indexToCharacters(2) = remaining25.filterNot(_ == indexToCharacters(5)).head


      var num = 0
      for (out <- fourteen.slice(10, 14)) {
        val digit = indexToCharacters.indexOf(out)
        if (digit == -1) throw new IllegalArgumentException("Digit not found")
        num = num * 10 + digit
      }
      ans += num
    }
    ans
  }

  def alphabetizeString(input: String): String = input.sorted

  def checkStringOverlap(larger: String, smaller: String): Boolean = {
    val largerSet = larger.toSet
    smaller.forall(largerSet.contains)
  }
}
