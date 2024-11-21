
import groovy.transform.Memoized

class Halves {
    boolean isChip
    String material

    @Override
    String toString() {
        String tType = isChip ? " microchip" : " generator"
        return "${material}${tType}"
    }
}

class State {
    List<List<Halves>> floors = [ [], [], [], [] ]
    int elevatorLevel = 0
    int steps = 0

    @Override
    String toString() {
        "Level ${elevatorLevel} x Steps ${steps}\n" + floors.indexed().collect { index, floor -> "  ${index}: ${floor}" }.join('\n')
    }

    @Memoized
    String hashKey() {
        def mapGenToIndex = [:]
        def mapChipToIndex = [:]
        floors.indexed().each { index, floor ->
            floor.each { half ->
                if (half.isChip) {
                    mapChipToIndex[half.material] = index
                } else {
                    mapGenToIndex[half.material] = index
                }
            }
        }

        def genChipPairs = mapGenToIndex.keySet().collect { material ->
            [mapGenToIndex[material], mapChipToIndex[material]]
        }

        genChipPairs.sort { a, b ->
            if (a[0] != b[0]) {
                a[0] <=> b[0]
            } else {
                a[1] <=> b[1]
            }
        }

        "${elevatorLevel}${genChipPairs}"
    }

    boolean isValid() {
        floors.each { floor ->
            def gensSeen = [:]
            floor.each { half ->
                if (!half.isChip) {
                    gensSeen[half.material] = true
                }
            }
            if (gensSeen) {
                floor.each { half ->
                    if (half.isChip && !gensSeen[half.material]) {
                        return false
                    }
                }
            }
        }
        true
    }

    boolean isDone() {
        floors[0..2].sum { it.size() } == 0
    }

    List<List<Integer>> getMovablePermIndices() {
        def currentLevel = floors[elevatorLevel]
        def permsToMove = []
        for (int i = 0; i < currentLevel.size(); i++) {
            for (int j = i + 1; j < currentLevel.size(); j++) {
                permsToMove << [i, j]
            }
            permsToMove << [i]
        }
        permsToMove
    }

    State clone() {
        new State(floors: floors.collect { it.clone() }, elevatorLevel: elevatorLevel, steps: steps)
    }

    List<State> getNextStates() {
        def futureStates = []
        def movablePermIndices = getMovablePermIndices()
        def eleDiffs = [elevatorLevel < 3 ? 1 : 0, elevatorLevel > 0 ? -1 : 0].findAll { it != 0 }

        eleDiffs.each { eleDiff ->
            movablePermIndices.each { permIndices ->
                def cl = clone()
                cl.elevatorLevel += eleDiff
                cl.steps++
                def oldLevel = elevatorLevel
                def newLevel = cl.elevatorLevel

                permIndices.each { index ->
                    cl.floors[newLevel] << cl.floors[oldLevel][index]
                }
                for (int in = permIndices.size() - 1; in >= 0; in--) {
                    cl.floors[oldLevel][permIndices[in]] = cl.floors[oldLevel].last()
                    cl.floors[oldLevel].removeLast()
                }

                if (cl.isValid()) {
                    futureStates << cl
                }
            }
        }
        futureStates
    }
}


def rtgHellDay(input) {
    def currentState = newInitialState(input)
    currentState.floors[0] += [
            new Halves(isChip: false, material: "elerium"),
            new Halves(isChip: true, material: "elerium"),
            new Halves(isChip: false, material: "dilithium"),
            new Halves(isChip: true, material: "dilithium"),
    ]

    def queue = [currentState]
    def prevStates = [:]
    while (queue) {
        def front = queue.remove(0)
        if (front.isDone()) {
            return front.steps
        }

        def hash = front.hashKey()
        if (prevStates[hash]) continue
        prevStates[hash] = true

        queue.addAll(front.getNextStates())
    }
    -1
}

def newInitialState(input) {
    def s = new State()
    input.readLines().indexed().each { lineIndex, line ->
        line.split().eachWithIndex { word, i ->
            if (word == "generator") {
                s.floors[lineIndex] << new Halves(isChip: false, material: line.split()[i - 1])
            } else if (word == "microchip") {
                s.floors[lineIndex] << new Halves(isChip: true, material: line.split()[i - 1].split("-")[0])
            }
        }
    }
    s
}


def input = new File("input.txt").text
println rtgHellDay(input)
