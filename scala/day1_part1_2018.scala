object Day1 {
    def main(args: Array[String]): Unit = {
        val input = scala.io.Source.fromFile("input.txt").getLines().toList
        val result = input.map(_.toInt).sum
        println(result)
    }
}