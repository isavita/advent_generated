
import java.io.File
import scala.io.Source

object Solution extends App {
  val input: String = Source.fromFile("input.txt").mkString
  println(visited(input, 10))

  def visited(input: String, ropelen: Int): Int = {
    val rope: Array[(Int, Int)] = Array.fill(ropelen)((0, 0))
    var visited: Set[(Int, Int)] = Set()
    for (line <- input.split("\n")) {
      val Array(b, n) = line.split(" ")
      val d: (Int, Int) = dirFromByte(b.head)
      for (_ <- 0 until n.toInt) {
        rope(0) = (rope(0)._1 + d._1, rope(0)._2 + d._2)
        for (j <- 1 until ropelen) {
          rope(j) = next(rope(j - 1), rope(j))
        }
        visited += rope(ropelen - 1)
      }
    }
    visited.size
  }

  def next(head: (Int, Int), tail: (Int, Int)): (Int, Int) = {
    if (Math.abs(head._1 - tail._1) <= 1 && Math.abs(head._2 - tail._2) <= 1) tail
    else (tail._1 + Math.signum(head._1 - tail._1).toInt, tail._2 + Math.signum(head._2 - tail._2).toInt)
  }

  def dirFromByte(b: Char): (Int, Int) = {
    b match {
      case 'N' => (0, 1)
      case 'E' => (1, 0)
      case 'S' => (0, -1)
      case 'W' => (-1, 0)
      case 'U' => (0, 1)
      case 'R' => (1, 0)
      case 'D' => (0, -1)
      case 'L' => (-1, 0)
      case '^' => (0, 1)
      case '>' => (1, 0)
      case 'v' => (0, -1)
      case '<' => (-1, 0)
    }
  }
}
