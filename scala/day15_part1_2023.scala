object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString.split(",")

  def hash(str: String): Int = {
    var current = 0
    for (char <- str) {
      current += char.toInt
      current *= 17
      current %= 256
    }
    current
  }

  val result = input.map(step => hash(step)).sum
  println(result)
}