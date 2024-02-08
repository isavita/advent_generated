object Main extends App {
  val lines = scala.io.Source.fromFile("input.txt").getLines().toList
  var x = List(1)
  for (line <- lines) {
    line match {
      case "noop" =>
        x = x :+ x.last
      case _ =>
        val n = line.split(" ").last.toInt
        x = x :+ x.last :+ (x.last + n)
    }
  }

  var sum = 0
  for (i <- x.indices) {
    if ((i - 19) % 40 == 0) {
      sum += (i + 1) * x(i)
    }
  }
  println(sum)
}