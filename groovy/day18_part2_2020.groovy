
class Day18 {

    static long evaluate(String expression, boolean advanced) {
        while (expression.contains("(")) {
            int open = -1
            int close = -1
            int openCount = 0
            for (int i = 0; i < expression.length(); i++) {
                if (expression[i] == '(') {
                    if (openCount == 0) {
                        open = i
                    }
                    openCount++
                } else if (expression[i] == ')') {
                    openCount--
                    if (openCount == 0) {
                        close = i
                        break
                    }
                }
            }
            long subResult = evaluate(expression.substring(open + 1, close), advanced)
            expression = expression.substring(0, open) + subResult + expression.substring(close + 1)
        }

        if (advanced) {
             while (expression.contains("+")) {
                def matcher = (expression =~ /(\d+)\s*\+\s*(\d+)/)
                if (matcher.find()) {
                    long sum = matcher[0][1].toLong() + matcher[0][2].toLong()
                    expression = expression.replaceFirst(java.util.regex.Pattern.quote(matcher[0][0]), String.valueOf(sum))
                }
            }
        }
        
        String[] parts = expression.split(" ")
        long result = parts[0].toLong()
        for (int i = 1; i < parts.length; i += 2) {
            String op = parts[i]
            long num = parts[i + 1].toLong()
            if (op == "+") {
                result += num
            } else if (op == "*") {
                result *= num
            }
        }
        return result
    }


    static void main(String[] args) {
        File inputFile = new File("input.txt")
        if (!inputFile.exists()) {
            println "Error: input.txt does not exist."
            return
        }

        long sumPart1 = 0
        long sumPart2 = 0

        inputFile.eachLine { line ->
            sumPart1 += evaluate(line, false)
            sumPart2 += evaluate(line, true)
        }

        println "Part 1: ${sumPart1}"
        println "Part 2: ${sumPart2}"
    }
}
