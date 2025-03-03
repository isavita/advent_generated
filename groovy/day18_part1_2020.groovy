
class Day18 {

    static long evaluate(String expression) {
        return evaluateExpression(tokenize(expression))
    }

    // Tokenizes the expression string into a list of numbers, operators, and parentheses.
    static List<String> tokenize(String expression) {
        return expression.replaceAll("\\s+", "").split("(?<=[()+*])|(?=[()+*])").findAll { it != "" }
    }

    // Evaluates a tokenized expression using a stack-based approach.  Crucially handles precedence.
    static long evaluateExpression(List<String> tokens) {
        Deque<Long> values = new ArrayDeque<>()
        Deque<String> operators = new ArrayDeque<>()

        for (String token : tokens) {
            switch (token) {
                case "(":
                    operators.push(token)
                    break
                case ")":
                    while (operators.peek() != "(") {
                        applyOperator(values, operators)
                    }
                    operators.pop() // Remove the opening parenthesis
                    break
                case "+":
                case "*":
                    // Apply any pending operators with equal or higher precedence (which is all of them in this case)
                    while (!operators.isEmpty() && operators.peek() != "(") {
                        applyOperator(values, operators)
                    }
                    operators.push(token)
                    break
                default: // It's a number
                    values.push(Long.parseLong(token))
            }
        }

        // Apply any remaining operators
        while (!operators.isEmpty()) {
            applyOperator(values, operators)
        }

        return values.pop()
    }


    // Helper function to apply the top operator from the operator stack to the top two values on the value stack.
    static void applyOperator(Deque<Long> values, Deque<String> operators) {
        long val2 = values.pop()
        long val1 = values.pop()
        String op = operators.pop()

        switch (op) {
            case "+":
                values.push(val1 + val2)
                break
            case "*":
                values.push(val1 * val2)
                break
        }
    }


    static void main(String[] args) {
        File inputFile = new File("input.txt")

        if (!inputFile.exists()) {
            println "Error: input.txt does not exist."
            return
        }

        long sum = 0
        inputFile.eachLine { line ->
            sum += evaluate(line)
        }
        println sum
    }
}
