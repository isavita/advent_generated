
class Monkey {
    String name
    def val
    boolean hasVal = false
    Monkey left
    Monkey right
    String op

    Monkey(String name) {
        this.name = name
    }

    def solve() {
        if (hasVal) {
            return [val, true]
        }

        if (left != null && right != null) {
            def (leftVal, lOk) = left.solve()
            def (rightVal, rOk) = right.solve()

            if (lOk && rOk) {
                switch (op) {
                    case "+": return [leftVal + rightVal, true]
                    case "-": return [leftVal - rightVal, true]
                    case "*": return [leftVal * rightVal, true]
                    case "/": return [leftVal / rightVal, true]
                    case "==": return [leftVal == rightVal ? 0 : 1, true]
                }
            }
        }
        return [0, false]
    }

    def expect(x) {
        if (name == "humn") {
            return x
        }

        def (leftVal, lOk) = left.solve()
        def (rightVal, rOk) = right.solve()

        if (!lOk) {
            switch (op) {
                case "+": return left.expect(x - rightVal)
                case "-": return left.expect(x + rightVal)
                case "*": return left.expect(x / rightVal)
                case "/": return left.expect(x * rightVal)
                case "==": return left.expect(rightVal)
            }
        }

        if (!rOk) {
            switch (op) {
                case "+": return right.expect(x - leftVal)
                case "-": return right.expect(leftVal - x)
                case "*": return right.expect(x / leftVal)
                case "/": return right.expect(leftVal / x)
                case "==": return right.expect(leftVal)
            }
        }
    }
}

def parse() {
    def index = [:]
    new File("input.txt").eachLine { line ->
        def parts = line.split(": ")
        def goal = parts[0]
        if (!index.containsKey(goal)) {
            index[goal] = new Monkey(goal)
        }

        if (parts[1].isNumber()) {
            index[goal].val = parts[1].toInteger()
            index[goal].hasVal = true
        } else {
            def r = parts[1].split()
            def leftName = r[0]
            def op = r[1]
            def rightName = r[2]

            if (!index.containsKey(leftName)) {
                index[leftName] = new Monkey(leftName)
            }
            if (!index.containsKey(rightName)) {
                index[rightName] = new Monkey(rightName)
            }

            index[goal].left = index[leftName]
            index[goal].op = op
            index[goal].right = index[rightName]
        }
    }
    index
}

def index = parse()
index["humn"].hasVal = false
index["root"].op = "=="
println index["root"].expect(0)
