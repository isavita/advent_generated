import scala.io.Source
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().take(2).map(_.toLong)
    val Seq(genAStart, genBStart) = lines.toSeq // Convert Iterator to Seq

    val genAFactor = 16807L
    val genBFactor = 48271L
    val modulus = 2147483647L

    var genA = genAStart
    var genB = genBStart
    var matches = 0

    for (_ <- 0 until 5000000) {
      genA = (genA * genAFactor) % modulus
      while (genA % 4 != 0) {
        genA = (genA * genAFactor) % modulus
      }

      genB = (genB * genBFactor) % modulus
      while (genB % 8 != 0) {
        genB = (genB * genBFactor) % modulus
      }

      if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
        matches += 1
      }
    }

    println(matches)
  }
}