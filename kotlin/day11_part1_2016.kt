import java.io.File
import java.util.LinkedList
import java.util.Queue

data class Halves(val isChip: Boolean, val material: String)

data class State(
    val floors: Array<MutableList<Halves>>,
    var elevatorLevel: Int,
    var steps: Int
) {
    fun hashKey(): String {
        val mapGenToIndex = mutableMapOf<String, Int>()
        val mapChipToIndex = mutableMapOf<String, Int>()
        floors.forEachIndexed { flIndex, fl ->
            fl.forEach { half ->
                if (half.isChip) mapChipToIndex[half.material] = flIndex
                else mapGenToIndex[half.material] = flIndex
            }
        }

        val genChipPairs = mutableListOf<Pair<Int, Int>>()
        mapGenToIndex.forEach { (material, index) ->
            genChipPairs.add(index to mapChipToIndex.getOrDefault(material, -1))
        }
        genChipPairs.sortWith(compareBy({ it.first }, { it.second }))

        return "$elevatorLevel$genChipPairs"
    }

    fun isValid(): Boolean {
        floors.forEach { fl ->
            val gensSeen = mutableSetOf<String>()
            fl.forEach { half ->
                if (!half.isChip) gensSeen.add(half.material)
            }
            if (gensSeen.isNotEmpty()) {
                fl.forEach { half ->
                    if (half.isChip && !gensSeen.contains(half.material)) return false
                }
            }
        }
        return true
    }

    fun isDone(): Boolean {
        return floors.take(3).sumOf { it.size } == 0
    }

    fun getMovablePermIndices(): List<List<Int>> {
        val permsToMove = mutableListOf<List<Int>>()
        val currentLevel = floors[elevatorLevel]
        for (i in 0 until currentLevel.size) {
            for (j in i + 1 until currentLevel.size) {
                permsToMove.add(listOf(i, j))
            }
            permsToMove.add(listOf(i))
        }
        return permsToMove
    }

    fun clone(): State {
        return State(Array(4) { floors[it].toMutableList() }, elevatorLevel, steps)
    }

    fun getNextStates(): List<State> {
        val futureStates = mutableListOf<State>()
        val movablePermIndices = getMovablePermIndices()
        val eleDiffs = mutableListOf<Int>()
        if (elevatorLevel < floors.size - 1) eleDiffs.add(1)
        if (elevatorLevel > 0) eleDiffs.add(-1)

        eleDiffs.forEach { eleDiff ->
            movablePermIndices.forEach { permIndices ->
                val cl = clone()
                cl.elevatorLevel += eleDiff
                cl.steps++
                val oldLevel = elevatorLevel
                val newLevel = cl.elevatorLevel

                permIndices.forEach { index ->
                    cl.floors[newLevel].add(cl.floors[oldLevel][index])
                }

                permIndices.sortedDescending().forEach { index ->
                    cl.floors[oldLevel].removeAt(index)
                }

                if (cl.isValid()) futureStates.add(cl)
            }
        }
        return futureStates
    }
}

fun newInitialState(input: String): State {
    val s = State(Array(4) { mutableListOf() }, 0, 0)
    input.split("\n").forEachIndexed { lineIndex, line ->
        val parts = line.split(" ").map { it.trim(',', '.') }
        for (i in parts.indices) {
            if (parts[i] == "generator") {
                s.floors[lineIndex].add(Halves(false, parts[i - 1]))
            } else if (parts[i] == "microchip") {
                s.floors[lineIndex].add(Halves(true, parts[i - 1].removeSuffix("-compatible")))
            }
        }
    }
    return s
}

fun rtgHellDay(input: String): Int {
    val currentState = newInitialState(input)
    val queue: Queue<State> = LinkedList()
    queue.add(currentState)
    val prevStates = mutableSetOf<String>()

    while (queue.isNotEmpty()) {
        val front = queue.poll()
        if (front.isDone()) return front.steps

        val hash = front.hashKey()
        if (prevStates.contains(hash)) continue
        prevStates.add(hash)

        queue.addAll(front.getNextStates())
    }
    return -1
}

fun main() {
    val input = File("input.txt").readText()
    println(rtgHellDay(input))
}