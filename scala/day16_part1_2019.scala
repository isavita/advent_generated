object Solution extends App {
  import scala.io.Source

  val source = Source.fromFile("input.txt")
  val input = source.getLines().mkString
  source.close()

  var digits = input.map(_.asDigit).toArray

  for (_ <- 0 until 100) {
    digits = applyFFT(digits)
  }

  println(digits.take(8).mkString)
}

def applyFFT(input: Array[Int]): Array[Int] = {
  val basePattern = Array(0, 1, 0, -1)
  val output = new Array[Int](input.length)

  for (i <- input.indices) {
    var sum = 0
    for (j <- input.indices) {
      val patternValue = basePattern(((j + 1) / (i + 1)) % basePattern.length)
      sum += input(j) * patternValue
    }
    output(i) = Math.abs(sum % 10)
  }

  output
}