
def solve() {
    def totalCalibrationResult = 0
    new File('input.txt').eachLine { line ->
        def parts = line.split(': ')
        def target = parts[0].toLong()
        def numbers = parts[1].split(' ').collect { it.toLong() }

        if (canBeMade(numbers, target)) {
            totalCalibrationResult += target
        }
    }
    println totalCalibrationResult
}

def canBeMade(numbers, target) {
    if (numbers.size() == 1) {
        return numbers[0] == target
    }

    def operators = ['+', '*', '||']
    def combinations = generateOperatorCombinations(numbers.size() - 1, operators)

    combinations.any { ops ->
        def result = numbers[0]
        for (int i = 0; i < ops.size(); i++) {
            def op = ops[i]
            def nextNum = numbers[i + 1]
            switch (op) {
                case '+':
                    result += nextNum
                    break
                case '*':
                    result *= nextNum
                    break
                case '||':
                    result = Long.parseLong(result.toString() + nextNum.toString())
                    break
            }
        }
        result == target
    }
}

def generateOperatorCombinations(numOperators, operators) {
    if (numOperators == 0) {
        return [[]]
    }

    def combinations = []
    def subCombinations = generateOperatorCombinations(numOperators - 1, operators)
    for (def op : operators) {
        for (def subCombination : subCombinations) {
            combinations << ([op] + subCombination)
        }
    }
    return combinations
}

solve()
