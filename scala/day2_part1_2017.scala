
object Day2 {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().toList
    val checksum = input.map(row => row.split("\\s+").map(_.toInt).toList).map(row => row.max - row.min).sum
    println(checksum)
  }
}
