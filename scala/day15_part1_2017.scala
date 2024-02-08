object Solution extends App {
  import scala.io.Source

  val bufferedSource = Source.fromFile("input.txt")
  val lines = bufferedSource.getLines().toList
  bufferedSource.close()

  val genAStart = lines(0).toLong
  val genBStart = lines(1).toLong

  val genAFactor = 16807
  val genBFactor = 48271
  val modulus = 2147483647

  var genA = genAStart
  var genB = genBStart
  var matches = 0

  for (_ <- 0 until 40000000) {
    genA = (genA * genAFactor) % modulus
    genB = (genB * genBFactor) % modulus

    if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
      matches += 1
    }
  }

  println(matches)
}