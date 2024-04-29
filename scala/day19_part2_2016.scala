import scala.io.Source

object Elephant {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val ans = elephant(input.toInt)
    println(ans)
  }

  case class LLNode(var elfNum: Int, var presents: Int, var next: LLNode = null)

  def elephant(startingElves: Int): Int = {
    var root = new LLNode(1, 1)
    var iter = root
    for (i <- 2 to startingElves) {
      iter.next = new LLNode(i, 1)
      iter = iter.next
    }
    iter.next = root

    var isOddLength = startingElves % 2 == 1
    var beforeAcross = root
    for (_ <- 0 until startingElves / 2 - 1) {
      beforeAcross = beforeAcross.next
    }

    while (root.next != root) {
      root.presents += beforeAcross.next.presents
      beforeAcross.next = beforeAcross.next.next
      if (isOddLength) beforeAcross = beforeAcross.next
      isOddLength = !isOddLength
      root = root.next
    }

    root.elfNum
  }
}