
import scala.io.Source
import scala.collection.mutable.Stack

object Solution extends App {

  val pairings = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
  val scoreValues = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
  val opening = "([{<"
  val closing = ")]}>"

  def checkAndCompleteLine(line: String): Option[Long] = {
    val stack = Stack[Char]()
    var corrupted = false

    for (char <- line) {
      if (opening.contains(char)) {
        stack.push(char)
      } else if (closing.contains(char)) {
        if (stack.isEmpty || stack.pop() != pairings(char)) {
          corrupted = true
          return None
        }
      }
    }

    if (stack.isEmpty) None
    else {
      var score: Long = 0
      while (stack.nonEmpty) {
        score = score * 5 + scoreValues(getClosingChar(stack.pop()))
      }
      Some(score)
    }
  }

  def getClosingChar(openingChar: Char): Char = openingChar match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    case _ => ' '
  }


  val scores = Source.fromFile("input.txt").getLines().flatMap(checkAndCompleteLine).toSeq
  scores.sorted
  println(scores(scores.size / 2))

}
