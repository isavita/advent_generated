
import scala.io.Source

object LinenLayout {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("input.txt")
    val lines = source.getLines().toList
    source.close()

    val patterns = lines.head.split(",").map(_.trim).toSet
    val designs = lines.tail.tail.map(_.trim)

    val possibleDesigns = designs.count(canFormDesign(_, patterns))

    println(s"Number of possible designs: $possibleDesigns")
  }

  def canFormDesign(design: String, patterns: Set[String]): Boolean = {
    val dp = Array.fill(design.length + 1)(false)
    dp(0) = true

    for (i <- 1 to design.length) {
      for (j <- 0 until i) {
        if (dp(j) && patterns.contains(design.substring(j, i))) {
          dp(i) = true
        }
      }
    }

    dp(design.length)
  }
}
