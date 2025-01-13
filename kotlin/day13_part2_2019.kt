
import java.io.File

fun main() {
    val program = File("input.txt").readText().trim().split(",").map { it.toLong() }.toMutableList()

    // Part 1
    val grid = mutableMapOf<Pair<Int, Int>, Int>()
    runGame(program.toMutableList(), grid, false)
    println(grid.values.count { it == 2 })

    // Part 2
    program[0] = 2
    val score = runGame(program.toMutableList(), mutableMapOf(), true)
    println(score)
}

fun runGame(program: MutableList<Long>, grid: MutableMap<Pair<Int, Int>, Int>, playForFree: Boolean): Long {
    val computer = IntcodeComputer(program)
    var score = 0L
    var paddleX = 0
    var ballX = 0

    while (!computer.isHalted) {
        if (playForFree) {
            computer.input = when {
                ballX < paddleX -> -1
                ballX > paddleX -> 1
                else -> 0
            }
        }

        val x = computer.run().toInt()
        if (computer.isHalted) break
        val y = computer.run().toInt()
        if (computer.isHalted) break
        val tileId = computer.run().toInt()

        if (x == -1 && y == 0) {
            score = tileId.toLong()
        } else {
            grid[Pair(x, y)] = tileId
            if (tileId == 3) paddleX = x
            if (tileId == 4) ballX = x
        }
    }
    return score
}

class IntcodeComputer(private val memory: MutableList<Long>) {
    private var ip = 0
    private var relativeBase = 0
    var input = 0L
    var isHalted = false

    fun run(): Long {
        while (true) {
            val opcode = memory[ip] % 100
            val modes = (memory[ip] / 100).toString().padStart(3, '0').reversed()

            fun getParam(index: Int): Long {
                val address = when (modes[index - 1]) {
                    '0' -> memory.getOrElse(ip + index) { 0 }.toInt()
                    '1' -> ip + index
                    '2' -> memory.getOrElse(ip + index) { 0 }.toInt() + relativeBase
                    else -> error("Invalid parameter mode")
                }
                return memory.getOrElse(address) { 0 }
            }

            fun setParam(index: Int, value: Long) {
                val address = when (modes[index - 1]) {
                    '0' -> memory.getOrElse(ip + index) { 0 }.toInt()
                    '2' -> memory.getOrElse(ip + index) { 0 }.toInt() + relativeBase
                    else -> error("Invalid parameter mode for writing")
                }
                if (address >= memory.size) {
                    memory.addAll(List(address - memory.size + 1) { 0 })
                }
                memory[address] = value
            }

            when (opcode) {
                1L -> {
                    setParam(3, getParam(1) + getParam(2))
                    ip += 4
                }
                2L -> {
                    setParam(3, getParam(1) * getParam(2))
                    ip += 4
                }
                3L -> {
                    setParam(1, input)
                    ip += 2
                }
                4L -> {
                    val output = getParam(1)
                    ip += 2
                    return output
                }
                5L -> {
                    if (getParam(1) != 0L) ip = getParam(2).toInt() else ip += 3
                }
                6L -> {
                    if (getParam(1) == 0L) ip = getParam(2).toInt() else ip += 3
                }
                7L -> {
                    setParam(3, if (getParam(1) < getParam(2)) 1 else 0)
                    ip += 4
                }
                8L -> {
                    setParam(3, if (getParam(1) == getParam(2)) 1 else 0)
                    ip += 4
                }
                9L -> {
                    relativeBase += getParam(1).toInt()
                    ip += 2
                }
                99L -> {
                    isHalted = true
                    return -1
                }
                else -> error("Invalid opcode: $opcode")
            }
        }
    }
}
