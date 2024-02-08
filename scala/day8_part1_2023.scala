object Solution extends App {
  import scala.io.Source

  case class Instruction(left: String, right: String)

  val ElemToMatch = "ZZZ"

  val input = Source.fromFile("input.txt").getLines.mkString("\n")

  val re = "[A-Z]{3}".r

  val lines = input.split("\n")

  val desertMap = lines.drop(2)
    .filter(_.nonEmpty)
    .map(line => re.findAllMatchIn(line).map(_.group(0)).toSeq)
    .map(matches => matches.head -> Instruction(matches(1), matches(2)))
    .toMap

  var current = "AAA"
  var steps = 0

  while (current != ElemToMatch) {
    for (i <- lines(0).trim.indices) {
      val direction = lines(0)(i)
      if (direction == 'R') {
        current = desertMap(current).right
      } else if (direction == 'L') {
        current = desertMap(current).left
      }
      steps += 1

      if (current == ElemToMatch) {
        // Break out of the loop if the element is found
        ()
      }
    }
  }

  println(steps)
}