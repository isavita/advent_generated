import java.io.File

fun main() {
    val ans = rtgHellDay(readFile("input.txt"))
    println(ans)
}

fun rtgHellDay(input: String): Int {
    val currentState = newInitialState(input)
    currentState.floors[0].addAll(listOf(
        Halves(false, "elerium"),
        Halves(true, "elerium"),
        Halves(false, "dilithium"),
        Halves(true, "dilithium")
    ))

    val queue = mutableListOf(currentState)
    val prevStates = mutableSetOf<String>()
    while (queue.isNotEmpty()) {
        val front = queue.removeAt(0)

        if (front.isDone()) {
            return front.steps
        }

        val hash = front.hashKey()
        if (prevStates.contains(hash)) {
            continue
        }
        prevStates.add(hash)

        queue.addAll(front.getNextStates())
    }

    return -1
}

data class Halves(val isChip: Boolean, val material: String) {
    override fun toString(): String {
        val tType = if (isChip) " microchip" else " generator"
        return "$material$tType"
    }
}

data class State(val floors: Array<MutableList<Halves>>, var elevatorLevel: Int, var steps: Int) {
    fun isValid(): Boolean {
        for (i in floors.indices) {
            val gensSeen = mutableMapOf<String, Boolean>()
            for (half in floors[i]) {
                if (!half.isChip) {
                    gensSeen[half.material] = true
                }
            }

            if (gensSeen.isEmpty()) {
                continue
            }

            for (half in floors[i]) {
                if (half.isChip && !gensSeen.containsKey(half.material)) {
                    return false
                }
            }
        }

        return true
    }

    fun isDone(): Boolean {
        var lenSum = 0
        for (fl in floors.sliceArray(0..2)) {
            lenSum += fl.size
        }
        return lenSum == 0
    }

    fun getMovablePermIndices(): List<List<Int>> {
        val permsToMove = mutableListOf<List<Int>>()

        val currentLevel = floors[elevatorLevel]

        for (i in 0 until currentLevel.size) {
            for (j in i + 1 until currentLevel.size) {
                permsToMove.add(listOf(i, j))
            }
        }

        for (i in 0 until currentLevel.size) {
            permsToMove.add(listOf(i))
        }
        return permsToMove
    }

    fun clone(): State {
        val cl = State(Array(4) { mutableListOf() }, elevatorLevel, steps)
        for (i in floors.indices) {
            cl.floors[i].addAll(floors[i])
        }
        return cl
    }

    fun getNextStates(): List<State> {
        val futureStates = mutableListOf<State>()

        val movablePermIndices = getMovablePermIndices()

        val eleDiffs = mutableListOf<Int>()
        if (elevatorLevel < floors.size - 1) {
            eleDiffs.add(1)
        }
        if (elevatorLevel > 0) {
            eleDiffs.add(-1)
        }

        for (eleDiff in eleDiffs) {
            for (permIndices in movablePermIndices) {
                val cl = clone()
                cl.elevatorLevel += eleDiff
                cl.steps++
                val oldLevel = elevatorLevel
                val newLevel = cl.elevatorLevel

                for (index in permIndices) {
                    cl.floors[newLevel].add(cl.floors[oldLevel][index])
                }

                for (inIndex in permIndices.size - 1 downTo 0) {
                    cl.floors[oldLevel][permIndices[inIndex]] = cl.floors[oldLevel].last()
                    cl.floors[oldLevel].removeAt(cl.floors[oldLevel].size - 1)
                }

                if (cl.isValid()) {
                    futureStates.add(cl)
                }
            }
        }

        return futureStates
    }

    fun hashKey(): String {
        val mapGenToIndex = mutableMapOf<String, Int>()
        val mapChipToIndex = mutableMapOf<String, Int>()
        for (flIndex in floors.indices) {
            for (half in floors[flIndex]) {
                if (half.isChip) {
                    mapChipToIndex[half.material] = flIndex
                } else {
                    mapGenToIndex[half.material] = flIndex
                }
            }
        }

        val genChipPairs = mutableListOf<Pair<Int, Int>>()
        for (material in mapGenToIndex.keys) {
            genChipPairs.add(Pair(mapGenToIndex[material]!!, mapChipToIndex[material]!!))
        }

        genChipPairs.sortWith(compareBy({ it.first }, { it.second }))

        return "$elevatorLevel$genChipPairs"
    }
}

fun newInitialState(input: String): State {
    val s = State(Array(4) { mutableListOf() }, 0, 0)

    for ((lineIndex, line) in input.split("\n").withIndex()) {
        val parts = line.split(" ").map { it.trim(',', '.') }

        for ((i, word) in parts.withIndex()) {
            if (word == "generator") {
                val material = parts[i - 1]
                s.floors[lineIndex].add(Halves(false, material))
            } else if (word == "microchip") {
                val material = parts[i - 1].substringBefore("-comp")
                s.floors[lineIndex].add(Halves(true, material))
            }
        }
    }

    return s
}

fun readFile(fileName: String): String {
    return File(fileName).readText().trimEnd()
}