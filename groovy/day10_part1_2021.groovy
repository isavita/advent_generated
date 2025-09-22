class Main {
    static Integer checkLine(String line) {
        def stack = new ArrayDeque<Character>()
        int score = 0
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i)
            switch (ch) {
                case '(':
                case '[':
                case '{':
                case '<':
                    stack.push(ch)
                    break
                case ')':
                case ']':
                case '}':
                case '>':
                    if (stack.isEmpty()) {
                        return null
                    }
                    char expected = (ch == ')') ? '(' : (ch == ']') ? '[' : (ch == '}') ? '{' : '<'
                    if (stack.peek() != expected) {
                        switch (ch) {
                            case ')':
                                score = 3
                                break
                            case ']':
                                score = 57
                                break
                            case '}':
                                score = 1197
                                break
                            case '>':
                                score = 25137
                                break
                        }
                        return score
                    }
                    stack.pop()
                    break
            }
        }
        return null
    }

    static void main(String[] args) {
        def file = new File("input.txt")
        if (!file.exists()) {
            System.err.println("Error opening file")
            System.exit(1)
        }
        int totalScore = 0
        file.eachLine { line ->
            Integer s = checkLine(line)
            if (s != null) totalScore += s
        }
        println totalScore
    }
}