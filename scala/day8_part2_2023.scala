
import scala.io.Source
import scala.util.Using

object Main {
  def parseInput(input: List[String]): (String, Map[String, (String, String)]) = {
    val instructions = input.head
    val nodes = input.drop(2).map { line =>
      val parts = line.split(" = ")
      val head = parts(0)
      val childrenTrim = parts(1).drop(1).dropRight(1)
      val childrenParts = childrenTrim.split(", ")
      (head, (childrenParts(0), childrenParts(1)))
    }.toMap
    (instructions, nodes)
  }

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)

  def lcmSlice(nums: List[Long]): Long = nums.reduce(lcm)

  def solve(input: List[String]): Long = {
    val (instructions, nodes) = parseInput(input)

    val starts = nodes.keys.filter(_.endsWith("A")).toList

    val steps = starts.map { start =>
      var element = start
      var step = 0L
      while (!element.endsWith("Z")) {
        val instruction = instructions((step % instructions.length).toInt)
        element = if (instruction == 'L') nodes(element)._1 else nodes(element)._2
        step += 1
      }
      step
    }
    lcmSlice(steps)
  }

  def readFile(fileName: String): List[String] = {
    Using(Source.fromFile(fileName)) { source =>
      source.getLines().toList
    }.get
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
