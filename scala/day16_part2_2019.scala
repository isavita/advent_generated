object Solution extends App {
  import scala.io.Source

  val source = Source.fromFile("input.txt")
  val input = source.getLines().next()
  source.close()

  val repeatedInput = repeatInput(input, 10000)

  val offset = input.slice(0, 7).toInt

  for (_ <- 0 until 100) {
    var sum = 0
    for (i <- repeatedInput.length - 1 to offset by -1) {
      sum += repeatedInput(i)
      repeatedInput(i) = sum % 10
    }
  }

  println(repeatedInput.slice(offset, offset + 8).mkString)

  def repeatInput(input: String, times: Int): Array[Int] = {
    val digits = Array.ofDim[Int](input.length * times)
    for {
      t <- 0 until times
      (r, i) <- input.zipWithIndex
    } {
      digits(t * input.length + i) = r.toString.toInt
    }
    digits
  }
}