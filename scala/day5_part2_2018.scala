object PolymerReduction {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").mkString
    val polymer = input.toCharArray

    val reactedPolymer = reactPolymer(polymer)
    println(s"Units remaining after reaction: ${reactedPolymer.length}")

    val shortestPolymerLength = (0 to 25).map { char =>
      val unitsToRemove = (char + 'a').toChar
      val polymerWithoutUnits = polymer.filter(c => c.toLower != unitsToRemove)
      reactPolymer(polymerWithoutUnits).length
    }.min

    println(s"Length of shortest polymer: $shortestPolymerLength")
  }

  def reactPolymer(polymer: Array[Char]): Array[Char] = {
    var stack = Array[Char]()
    for (c <- polymer) {
      if (stack.nonEmpty && reacts(stack.last, c)) {
        stack = stack.dropRight(1)
      } else {
        stack = stack :+ c
      }
    }
    stack
  }

  def reacts(c1: Char, c2: Char): Boolean = {
    c1.toLower == c2.toLower && c1 != c2
  }
}