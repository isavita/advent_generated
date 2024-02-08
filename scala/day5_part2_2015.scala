object Solution extends App {
  val source = io.Source.fromFile("input.txt")
  val input = try source.getLines.mkString("\n") finally source.close()

  var nice = 0
  def passesRule1(line: String): Boolean = {
    for {
      i <- 0 until line.length - 2
      toMatch = line.slice(i, i + 2)
      j <- i + 2 until line.length - 1
      if line.slice(j, j + 2) == toMatch
    } return true
    false
  }

  input.split("\n").foreach { line =>
    val rule1 = passesRule1(line)

    var rule2 = false
    for (i <- 0 until line.length - 2) {
      if (line(i) == line(i + 2)) {
        rule2 = true
      }
    }

    if (rule1 && rule2) {
      nice += 1
    }
  }

  println(nice)
}