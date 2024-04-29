def file = new File("input.txt")
def initialState
def rules = [:]

file.eachLine { line ->
    if (line.contains("initial state")) {
        initialState = line.split(": ")[1]
    } else if (line.contains("=>")) {
        def parts = line.split(" => ")
        rules[parts[0]] = parts[1][0]
    }
}

def state = [:]
initialState.eachWithIndex { c, i ->
    if (c == '#') {
        state[i] = '#'
    }
}

def previousPattern = ""
def previousSum = 0
def offset = 0
for (int generation = 0; generation < 50000000000; generation++) {
    def newState = [:]
    def minPot = state.keySet().min()
    def maxPot = state.keySet().max()
    for (int i = minPot - 2; i <= maxPot + 2; i++) {
        def pattern = ""
        for (int j = i - 2; j <= i + 2; j++) {
            if (state[j] == '#') {
                pattern += "#"
            } else {
                pattern += "."
            }
        }
        if (rules[pattern] == '#') {
            newState[i] = '#'
        }
    }
    state = newState

    def (currentPattern, currentSum) = statePattern(state)
    if (currentPattern == previousPattern) {
        offset = currentSum - previousSum
        def remainingGenerations = 50000000000 - generation - 1
        def finalSum = currentSum + offset * remainingGenerations
        println finalSum
        return
    }
    previousPattern = currentPattern
    previousSum = currentSum
}

def minMaxKeys(Map m) {
    def minKey = m.keySet().min()
    def maxKey = m.keySet().max()
    [minKey, maxKey]
}

def statePattern(Map m) {
    def minPot = m.keySet().min()
    def maxPot = m.keySet().max()
    def pattern = new StringBuilder()
    def sum = 0
    for (int i = minPot; i <= maxPot; i++) {
        if (m[i] == '#') {
            pattern << '#'
            sum += i
        } else {
            pattern << '.'
        }
    }
    [pattern.toString(), sum]
}