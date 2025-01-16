
import scala.io.Source

def nextSecret(s: Long): Long = {
  var x = s * 64
  var result = s ^ x
  result &= 0xFFFFFF
  x = result / 32
  result ^= x
  result &= 0xFFFFFF
  x = result * 2048
  result ^= x
  result &= 0xFFFFFF
  result
}

@main def main(): Unit = {
  val buyers = Source.fromFile("input.txt").getLines().filter(_.nonEmpty).map(_.toLong).toList
  val total = buyers.map { b =>
    var s = b
    for (_ <- 0 until 2000) {
      s = nextSecret(s)
    }
    s
  }.sum
  println(total)
}
