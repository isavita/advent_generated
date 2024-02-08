
object Day8 {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().toList
    val codeLength = input.map(_.length).sum
    val memoryLength = input.map(_.tail.init.replaceAll("\\\\\\\\", "1").replaceAll("\\\\\"", "2").replaceAll("\\\\x[0-9a-f]{2}", "3")).map(_.length).sum
    println(codeLength - memoryLength)
  }
}
