
class SnailNumber {
    Integer value
    SnailNumber left
    SnailNumber right

    boolean isRegular() {
        return left == null && right == null
    }

    SnailNumber add(SnailNumber other) {
        new SnailNumber(left: this.copy(), right: other.copy()).reduce()
    }
    
    SnailNumber copy() {
        if (isRegular()) {
            return new SnailNumber(value: value)
        } else {
            return new SnailNumber(left: left.copy(), right: right.copy())
        }
    }

    SnailNumber reduce() {
        while (true) {
            def exploded_left_right = explode(0)
            if (exploded_left_right[0]) continue
            if (!split()) break
        }
        this
    }

    List explode(int depth) {
        if (isRegular()) return [false, 0, 0]
        if (depth == 4) {
            def lv = left.value
            def rv = right.value
            left = null
            right = null
            value = 0
            return [true, lv, rv]
        }
        def exploded_left_right = left.explode(depth + 1)
        if (exploded_left_right[0]) {
            if (exploded_left_right[2] > 0 && right != null) right.addLeft(exploded_left_right[2])
            return [true, exploded_left_right[1], 0]
        }
        exploded_left_right = right.explode(depth + 1)
        if (exploded_left_right[0]) {
            if (exploded_left_right[1] > 0 && left != null) left.addRight(exploded_left_right[1])
            return [true, 0, exploded_left_right[2]]
        }
        [false, 0, 0]
    }

    void addLeft(int value) {
        if (isRegular()) this.value += value
        else left.addLeft(value)
    }

    void addRight(int value) {
        if (isRegular()) this.value += value
        else right.addRight(value)
    }

    boolean split() {
        if (isRegular()) {
            if (value >= 10) {
                left = new SnailNumber(value: value / 2)
                right = new SnailNumber(value: (value + 1) / 2)
                value = -1
                return true
            }
            return false
        }
        left.split() || right.split()
    }

    int magnitude() {
        isRegular() ? value : 3 * left.magnitude() + 2 * right.magnitude()
    }

    static SnailNumber parseSnailNumber(String input) {
        input = input.trim()
        if (input[0] != '[') return new SnailNumber(value: input.toInteger())
        def balance = 0
        def splitIndex = 0
        for (int i = 1; i < input.length() - 1; i++) {
            switch (input[i]) {
                case '[': balance++; break
                case ']': balance--; break
                case ',':
                    if (balance == 0) {
                        splitIndex = i
                        break
                    }
            }
            if (splitIndex != 0) break
        }
        def left = parseSnailNumber(input[1..<splitIndex])
        def right = parseSnailNumber(input[splitIndex + 1..<input.length() - 1])
        new SnailNumber(left: left, right: right)
    }
}

def file = new File("input.txt")
def snailNumbers = file.readLines().collect { SnailNumber.parseSnailNumber(it) }
def result = snailNumbers[0]
for (int i = 1; i < snailNumbers.size(); i++) result = result.add(snailNumbers[i])
println result.magnitude()
