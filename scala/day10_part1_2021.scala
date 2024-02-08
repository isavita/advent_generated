
object SyntaxScoring {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().toList

    def isOpening(c: Char): Boolean = c == '(' || c == '[' || c == '{' || c == '<'
    def isClosing(c: Char): Boolean = c == ')' || c == ']' || c == '}' || c == '>'

    def isMatching(open: Char, close: Char): Boolean =
      (open == '(' && close == ')') || (open == '[' && close == ']') || (open == '{' && close == '}') || (open == '<' && close == '>')

    def findSyntaxErrors(line: String): Int = {
      var errors = 0
      var stack = List[Char]()

      for (c <- line) {
        if (isOpening(c)) {
          stack = c :: stack
        } else if (isClosing(c)) {
          if (stack.isEmpty || !isMatching(stack.head, c)) {
            return if (c == ')') 3 else if (c == ']') 57 else if (c == '}') 1197 else 25137
          } else {
            stack = stack.tail
          }
        }
      }
      errors
    }

    val corruptedLines = input.filter(line => line.exists(c => isClosing(c) || isOpening(c)))
    val syntaxErrorScores = corruptedLines.map(findSyntaxErrors)

    println(syntaxErrorScores.sum)
  }
}
