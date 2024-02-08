object Solution extends App {
  import scala.io.Source

  val file = Source.fromFile("input.txt")
  val lines = file.getLines().toList
  file.close()

  var count = 0
  for (line <- lines) {
    val parts = line.split(" \\| ")
    val output = parts(1).split(" ")
    output.foreach {
      case digit if List(2, 4, 3, 7).contains(digit.length) => count += 1
      case _ =>
    }
  }

  println(count)
}