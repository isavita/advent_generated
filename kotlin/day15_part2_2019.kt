
import java.io.File
import java.util.LinkedList

fun main() {
    val program = File("input.txt").readText().trim().split(",").map { it.toLong() }.toMutableList()
    val grid = mutableMapOf<Pair<Int, Int>, Int>()
    val start = 0 to 0
    grid[start] = 1

    val (oxygenSystem, _) = explore(program, grid)
    val path = findPath(grid, start, oxygenSystem!!)
    println("Part 1: ${path.size - 1}")

    val minutes = fillWithOxygen(grid, oxygenSystem)
    println("Part 2: $minutes")
}

fun explore(program: MutableList<Long>, grid: MutableMap<Pair<Int, Int>, Int>): Pair<Pair<Int, Int>?, Map<Pair<Int, Int>, Int>> {
    val visited = mutableSetOf<Pair<Int, Int>>()
    val q = LinkedList<Triple<Pair<Int, Int>, List<Int>, IntcodeComputer>>()
    q.add(Triple(0 to 0, emptyList(), IntcodeComputer(program)))
    var oxygenSystem: Pair<Int, Int>? = null

    while (q.isNotEmpty()) {
        val (currPos, path, computer) = q.remove()
        if (currPos in visited) continue
        visited.add(currPos)

        for (dir in 1..4) {
            val newPos = move(currPos, dir)
            val newComputer = computer.copy()
            newComputer.input.add(dir.toLong())
            newComputer.run()
            val status = newComputer.output.removeAt(0).toInt()
            grid[newPos] = status

            if (status != 0) {
                if (status == 2) {
                    oxygenSystem = newPos
                }
                q.add(Triple(newPos, path + dir, newComputer))
            }
        }
    }
    return Pair(oxygenSystem, grid)
}

fun move(pos: Pair<Int, Int>, dir: Int): Pair<Int, Int> {
    return when (dir) {
        1 -> pos.first to pos.second + 1
        2 -> pos.first to pos.second - 1
        3 -> pos.first - 1 to pos.second
        4 -> pos.first + 1 to pos.second
        else -> throw IllegalArgumentException("Invalid direction")
    }
}

fun findPath(grid: Map<Pair<Int, Int>, Int>, start: Pair<Int, Int>, end: Pair<Int, Int>): List<Pair<Int, Int>> {
    val q = LinkedList<Pair<Pair<Int, Int>, List<Pair<Int, Int>>>>()
    q.add(start to listOf(start))
    val visited = mutableSetOf<Pair<Int, Int>>()

    while (q.isNotEmpty()) {
        val (curr, path) = q.remove()
        if (curr == end) return path
        if (curr in visited) continue
        visited.add(curr)

        for (dir in 1..4) {
            val next = move(curr, dir)
            if (grid[next] != 0) {
                q.add(next to path + next)
            }
        }
    }
    return emptyList()
}

fun fillWithOxygen(grid: MutableMap<Pair<Int, Int>, Int>, start: Pair<Int, Int>): Int {
    var minutes = 0
    val q = LinkedList<Pair<Int, Int>>()
    q.add(start)
    grid[start] = 3 

    while (true) {
        val nextMinute = LinkedList<Pair<Int, Int>>()
        while (q.isNotEmpty()) {
            val curr = q.remove()
            for (dir in 1..4) {
                val next = move(curr, dir)
                if (grid[next] == 1) {
                    grid[next] = 3
                    nextMinute.add(next)
                }
            }
        }
        if (nextMinute.isEmpty()) break
        q.addAll(nextMinute)
        minutes++
    }
    return minutes
}

class IntcodeComputer(program: List<Long>) {
    private val memory = program.toMutableList().also { it.addAll(List(10000) { 0L }) }
    private var ip = 0
    private var relativeBase = 0
    val input = mutableListOf<Long>()
    val output = mutableListOf<Long>()

    fun run(): IntcodeComputer {
        while (true) {
            val opcode = memory[ip] % 100
            val modes = (memory[ip] / 100).toString().padStart(3, '0').reversed()

            fun getParam(index: Int): Long {
                return when (modes[index - 1]) {
                    '0' -> memory[memory[ip + index].toInt()]
                    '1' -> memory[ip + index]
                    '2' -> memory[memory[ip + index].toInt() + relativeBase]
                    else -> throw IllegalArgumentException("Invalid parameter mode")
                }
            }

            fun setParam(index: Int, value: Long) {
                when (modes[index - 1]) {
                    '0' -> memory[memory[ip + index].toInt()] = value
                    '2' -> memory[memory[ip + index].toInt() + relativeBase] = value
                    else -> throw IllegalArgumentException("Invalid parameter mode for set")
                }
            }

            when (opcode.toInt()) {
                1 -> {
                    setParam(3, getParam(1) + getParam(2))
                    ip += 4
                }
                2 -> {
                    setParam(3, getParam(1) * getParam(2))
                    ip += 4
                }
                3 -> {
                    if (input.isEmpty()) return this
                    setParam(1, input.removeAt(0))
                    ip += 2
                }
                4 -> {
                    output.add(getParam(1))
                    ip += 2
                }
                5 -> {
                    if (getParam(1) != 0L) ip = getParam(2).toInt() else ip += 3
                }
                6 -> {
                    if (getParam(1) == 0L) ip = getParam(2).toInt() else ip += 3
                }
                7 -> {
                    setParam(3, if (getParam(1) < getParam(2)) 1 else 0)
                    ip += 4
                }
                8 -> {
                    setParam(3, if (getParam(1) == getParam(2)) 1 else 0)
                    ip += 4
                }
                9 -> {
                    relativeBase += getParam(1).toInt()
                    ip += 2
                }
                99 -> return this
                else -> throw IllegalArgumentException("Invalid opcode: $opcode")
            }
        }
    }

    fun copy(): IntcodeComputer {
        val newComputer = IntcodeComputer(memory)
        newComputer.ip = ip
        newComputer.relativeBase = relativeBase
        newComputer.input.addAll(input)
        newComputer.output.addAll(output)
        return newComputer
    }
}
