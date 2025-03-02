
import java.nio.file.Files
import java.nio.file.Paths

def solvePart1(initialState) {
    def state = initialState
    for (int i = 0; i < 10; i++) {
        state = nextState(state)
    }
    def woodedAcres = state.flatten().count { it == '|' }
    def lumberyards = state.flatten().count { it == '#' }
    return woodedAcres * lumberyards
}

def solvePart2(initialState) {
    def state = initialState
    def seenStates = [:]
    def results = []
    int periodStart = -1
    int periodLength = -1

    for (int i = 0; i < 1000; i++) {
        def stateString = state.collect { it.join() }.join("\n")
        if (seenStates.containsKey(stateString)) {
            periodStart = seenStates[stateString]
            periodLength = i - periodStart
            break
        }
        seenStates[stateString] = i
        results.add(stateString)

        state = nextState(state)
    }

    def remainingCycles = 1000000000 - periodStart
    def effectiveIndex = remainingCycles % periodLength + periodStart

    def finalStateString = seenStates.find { it.value == effectiveIndex }?.key
    def finalState = finalStateString.split("\n").collect { it.toList() }

    def woodedAcres = finalState.flatten().count { it == '|' }
    def lumberyards = finalState.flatten().count { it == '#' }

    return woodedAcres * lumberyards
}


def nextState(state) {
    def newState = state.collect { it.clone() }
    for (int y = 0; y < state.size(); y++) {
        for (int x = 0; x < state[0].size(); x++) {
            def neighbors = getNeighbors(state, x, y)
            def acre = state[y][x]

            if (acre == '.') {
                def treeCount = neighbors.count { it == '|' }
                if (treeCount >= 3) {
                    newState[y][x] = '|'
                }
            } else if (acre == '|') {
                def lumberyardCount = neighbors.count { it == '#' }
                if (lumberyardCount >= 3) {
                    newState[y][x] = '#'
                }
            } else if (acre == '#') {
                if (neighbors.contains('#') && neighbors.contains('|')) {
                    //Stays a lumberyard
                } else {
                    newState[y][x] = '.'
                }
            }
        }
    }
    return newState
}

def getNeighbors(state, x, y) {
    def neighbors = []
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            if (dx == 0 && dy == 0) continue
            def nx = x + dx
            def ny = y + dy
            if (nx >= 0 && nx < state[0].size() && ny >= 0 && ny < state.size()) {
                neighbors.add(state[ny][nx])
            }
        }
    }
    return neighbors
}

def main() {
    def lines = Files.readAllLines(Paths.get("input.txt"))
    def initialState = lines.collect { it.toList() }

    println "Part 1: " + solvePart1(initialState)
    println "Part 2: " + solvePart2(initialState)
}

main()
