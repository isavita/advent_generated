
import scala.io.Source

case class Scanner(range: Int, var position: Int, direction: Int)

object Solution {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val firewall = Source.fromFile(filename).getLines()
      .map(line => {
        val Array(depth, rng) = line.split(": ")
        (depth.toInt, Scanner(rng.toInt, 0, 1))
      })
      .toMap

    var delay = 0
    while (!passThrough(firewall, delay)) {
      delay += 1
    }

    println(delay)
  }

  def passThrough(firewall: Map[Int, Scanner], delay: Int): Boolean = {
    firewall.forall { case (depth, scanner) =>
      (depth + delay) % (2 * (scanner.range - 1)) != 0
    }
  }
}
