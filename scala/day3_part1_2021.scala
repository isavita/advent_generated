object Main extends App {
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.getLines().toList finally source.close()

  val counts = Array.ofDim[Int](12, 2)
  lines.foreach { line =>
    line.zipWithIndex.foreach { case (num, i) =>
      counts(i)(num.toInt - '0') += 1
    }
  }

  var gammaRate = 0
  var epsilonRate = 0
  counts.indices.foreach { i =>
    if (counts(i)(0) > counts(i)(1)) {
      gammaRate |= 1 << (counts.length - i - 1)
    } else {
      epsilonRate |= 1 << (counts.length - i - 1)
    }
  }

  println(gammaRate * epsilonRate)
}