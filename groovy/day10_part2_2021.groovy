class Main {
  static void main(String[] args) {
    def file = new File("input.txt")
    def scores = []
    if (file.exists()) {
      file.eachLine { line ->
        def s = checkAndCompleteLine(line)
        if (s != null) scores << s
      }
    }
    scores.sort()
    if (scores.size() > 0) {
      println scores[scores.size() / 2]
    } else {
      println 0
    }
  }

  static Long checkAndCompleteLine(String line) {
    def stack = []
    for (int i = 0; i < line.length(); i++) {
      char c = line.charAt(i)
      if (c == '(' || c == '[' || c == '{' || c == '<') {
        stack << c
      } else if (c == ')' || c == ']' || c == '}' || c == '>') {
        if (stack.isEmpty()) return null
        char top = stack.remove(stack.size() - 1)
        if (!((top == '(' && c == ')') || (top == '[' && c == ']') || (top == '{' && c == '}') || (top == '<' && c == '>'))) {
          return null
        }
      }
    }
    if (stack.isEmpty()) return null
    long score = 0L
    while (!stack.isEmpty()) {
      char opening = stack.remove(stack.size() - 1)
      score = score * 5 + getClosingCharValue(opening)
    }
    return score
  }

  static int getClosingCharValue(char openingChar) {
    switch (openingChar) {
      case '(':
        return 1
      case '[':
        return 2
      case '{':
        return 3
      case '<':
        return 4
      default:
        return 0
    }
  }
}