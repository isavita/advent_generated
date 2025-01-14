
class SnailNumber {
    Integer value
    SnailNumber left
    SnailNumber right

    boolean isRegular() {
        left == null && right == null
    }

    SnailNumber add(SnailNumber other) {
        new SnailNumber(left: this.deepCopy(), right: other.deepCopy()).reduce()
    }

    SnailNumber reduce() {
        while (true) {
            def exploded = explode(0)
            if (exploded[0]) continue
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
        def exploded = left.explode(depth + 1)
        if (exploded[0]) {
            if (exploded[2] > 0 && right != null) right.addLeft(exploded[2])
            return [true, exploded[1], 0]
        }
        exploded = right.explode(depth + 1)
        if (exploded[0]) {
            if (exploded[1] > 0 && left != null) left.addRight(exploded[1])
            return [true, 0, exploded[2]]
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

    SnailNumber deepCopy() {
        isRegular() ? new SnailNumber(value: value) : new SnailNumber(left: left.deepCopy(), right: right.deepCopy())
    }

    static SnailNumber parse(String input) {
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
        def left = parse(input[1..<splitIndex])
        def right = parse(input[splitIndex + 1..<input.length() - 1])
        new SnailNumber(left: left, right: right)
    }
}

def largestMagnitude = 0
def snailNumbers = []
new File("input.txt").eachLine { line ->
    snailNumbers << SnailNumber.parse(line)
}

for (int i = 0; i < snailNumbers.size(); i++) {
    for (int j = 0; j < snailNumbers.size(); j++) {
        if (i == j) continue
        def a = snailNumbers[i].deepCopy()
        def b = snailNumbers[j].deepCopy()
        
        def sum1 = a.add(b).magnitude()
        
        a = snailNumbers[i].deepCopy()
        b = snailNumbers[j].deepCopy()
        
        def sum2 = b.add(a).magnitude()

        largestMagnitude = Math.max(largestMagnitude, Math.max(sum1, sum2))
    }
}

println largestMagnitude
