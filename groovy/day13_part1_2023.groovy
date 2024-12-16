
def findReflection(pattern) {
    def rows = pattern.size()
    def cols = pattern[0].size()

    // Check for horizontal reflection
    for (int i = 0; i < rows - 1; i++) {
        def valid = true
        for (int j = 0; i - j >= 0 && i + j + 1 < rows; j++) {
            if (pattern[i - j] != pattern[i + j + 1]) {
                valid = false
                break
            }
        }
        if (valid) {
            return (i + 1) * 100
        }
    }

    // Check for vertical reflection
    for (int i = 0; i < cols - 1; i++) {
        def valid = true
        for (int j = 0; i - j >= 0 && i + j + 1 < cols; j++) {
            for (int k = 0; k < rows; k++) {
                if (pattern[k][i - j] != pattern[k][i + j + 1]) {
                    valid = false
                    break
                }
            }
            if (!valid) break
        }
        if (valid) {
            return i + 1
        }
    }
    return 0
}

def solve() {
    def patterns = []
    def currentPattern = []
    new File('input.txt').eachLine { line ->
        if (line.trim().isEmpty()) {
            if (currentPattern) {
                patterns << currentPattern
                currentPattern = []
            }
        } else {
            currentPattern << line.trim()
        }
    }
    if (currentPattern) {
        patterns << currentPattern
    }

    def total = 0
    patterns.each { pattern ->
        total += findReflection(pattern)
    }
    println total
}

solve()
