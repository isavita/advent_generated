
def parseInput(filePath) {
    def lines = new File(filePath).readLines()
    def initialState = lines[0][-2]
    def steps = lines[1] =~ /\d+/
    
    def states = [:]
    for (int i = 3; i < lines.size(); i += 10) {
        def state = lines[i][-2]
        def value0 = lines[i+2][-2] as int
        def move0 = lines[i+3].endsWith("left.") ? -1 : 1
        def nextState0 = lines[i+4][-2]
        def value1 = lines[i+6][-2] as int
        def move1 = lines[i+7].endsWith("left.") ? -1 : 1
        def nextState1 = lines[i+8][-2]
        states[state] = [(0): [value0, move0, nextState0], (1): [value1, move1, nextState1]]
    }
    return [initialState, steps[0] as int, states]
}

def runTuringMachine(filePath) {
    def (state, steps, states) = parseInput(filePath)
    def tape = [:]
    def cursor = 0
    def checksum = 0

    for (int i = 0; i < steps; i++) {
        def value = tape.containsKey(cursor) ? tape[cursor] : 0
        def newValue = states[state][value][0]
        def move = states[state][value][1]
        def nextState = states[state][value][2]

        tape[cursor] = newValue
        cursor += move
        state = nextState
    }

    tape.each { k, v -> checksum += v }
    return checksum
}

def result = runTuringMachine("input.txt")
println result
