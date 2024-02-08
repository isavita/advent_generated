
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.mkString
  var score = 0
  var level = 0
  var garbage = false
  var ignoreNext = false

  for (char <- input) {
    if (ignoreNext) {
      ignoreNext = false
    } else if (char == '!') {
      ignoreNext = true
    } else if (garbage) {
      if (char == '>') {
        garbage = false
      }
    } else {
      char match {
        case '{' => {
          level += 1
          score += level
        }
        case '}' => level -= 1
        case '<' => garbage = true
        case _ =>
      }
    }
  }

  println(score)
}
