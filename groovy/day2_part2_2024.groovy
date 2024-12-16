
def isSafe(List<Integer> levels) {
    if (levels.size() <= 1) return true

    def increasing = levels[1] > levels[0]
    for (int i = 1; i < levels.size(); i++) {
        def diff = levels[i] - levels[i - 1]
        if (diff == 0 || Math.abs(diff) > 3) return false
        if ((diff > 0) != increasing) return false
    }
    return true
}

def isSafeWithDampener(List<Integer> levels) {
    if (isSafe(levels)) return true
    for (int i = 0; i < levels.size(); i++) {
        def tempLevels = new ArrayList<>(levels)
        tempLevels.remove(i)
        if (isSafe(tempLevels)) return true
    }
    return false
}

def solve() {
    def safeCount = 0
    new File('input.txt').eachLine { line ->
        def levels = line.split(' ').collect { it.toInteger() }
        if (isSafeWithDampener(levels)) {
            safeCount++
        }
    }
    println safeCount
}

solve()
