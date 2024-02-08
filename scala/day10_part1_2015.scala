object Main extends App {
  val source = io.Source.fromFile("input.txt")
  val initialSequence = try source.getLines.mkString finally source.close()

  val result = lookAndSay(initialSequence, 40)
  println(result.length)

  def lookAndSay(sequence: String, iterations: Int): String = {
    (0 until iterations).foldLeft(sequence) { (seq, _) =>
      nextSequence(seq)
    }
  }

  def nextSequence(sequence: String): String = {
    val result = new StringBuilder
    var i = 0

    while (i < sequence.length) {
      var count = 1
      val digit = sequence(i)
      
      var j = i + 1
      while (j < sequence.length && sequence(j) == digit) {
        count += 1
        j += 1
      }
      
      result.append(s"$count$digit")
      i += count
    }
    
    result.toString()
  }
}