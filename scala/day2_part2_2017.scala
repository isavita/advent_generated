import scala.io.Source
import scala.util.Try

object Main extends App {
  val data = Source.fromFile("input.txt").mkString
  val lines = data.split("\n").map(_.trim)

  var sum = 0

  for (line <- lines) {
    val nums = line.split("\\s+").map(str => Try(str.toInt).toOption).flatten
    for (i <- nums; j <- nums if i != j && i % j == 0) {
      sum += i / j
    }
  }

  println(sum)
}