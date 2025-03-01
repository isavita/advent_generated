
import java.io.File
import java.util.LinkedList

fun main() {
    val program = File("input.txt").readText().trim().split(",").map { it.toLong() }

    val computers = List(50) { IntcodeComputer(program.toMutableList(), mutableListOf(it.toLong())) }
    val packetQueues = List(50) { LinkedList<Pair<Long, Long>>() }

    var firstPacketTo255: Long? = null

    while (true) {
        var idle = true
        for ((i, computer) in computers.withIndex()) {
            if (packetQueues[i].isNotEmpty()) {
                val (x, y) = packetQueues[i].removeFirst()
                computer.inputs.addAll(listOf(x, y))
            } else {
                computer.inputs.add(-1)
            }

            computer.run()

            while (computer.outputs.size >= 3) {
                idle = false
                val dest = computer.outputs.removeAt(0).toInt()
                val x = computer.outputs.removeAt(0)
                val y = computer.outputs.removeAt(0)

                if (dest == 255) {
                    if (firstPacketTo255 == null) {
                        firstPacketTo255 = y
                        println(firstPacketTo255)
                        return
                    }
                } else if (dest in 0..49) {
                    packetQueues[dest].add(Pair(x, y))
                }
            }
        }
    }
}

class IntcodeComputer(program: MutableList<Long>, val inputs: MutableList<Long>) {
    private val memory = program.withIndex().associate { it.index.toLong() to it.value }.toMutableMap()
    private var ip = 0L
    private var relativeBase = 0L
    val outputs = mutableListOf<Long>()
    var halted = false
    var needsInput = false

    private fun getParam(mode: Int, offset: Int): Long {
        return when (mode) {
            0 -> memory.getOrDefault(memory.getOrDefault(ip + offset, 0), 0)
            1 -> memory.getOrDefault(ip + offset, 0)
            2 -> memory.getOrDefault(relativeBase + memory.getOrDefault(ip + offset, 0), 0)
            else -> error("Unknown mode $mode")
        }
    }

    private fun setParam(mode: Int, offset: Int, value: Long) {
        when (mode) {
            0 -> memory[memory.getOrDefault(ip + offset, 0)] = value
            2 -> memory[relativeBase + memory.getOrDefault(ip + offset, 0)] = value
            else -> error("Unknown mode $mode")
        }
    }

    fun run() {
        while (true) {
            val opcode = (memory.getOrDefault(ip, 0) % 100).toInt()
            val modes = listOf(
                (memory.getOrDefault(ip, 0) / 100 % 10).toInt(),
                (memory.getOrDefault(ip, 0) / 1000 % 10).toInt(),
                (memory.getOrDefault(ip, 0) / 10000 % 10).toInt()
            )

            if (opcode == 99) {
                halted = true
                break
            }

            when (opcode) {
                1, 2, 7, 8 -> {
                    val param1 = getParam(modes[0], 1)
                    val param2 = getParam(modes[1], 2)
                    val result = when (opcode) {
                        1 -> param1 + param2
                        2 -> param1 * param2
                        7 -> if (param1 < param2) 1 else 0
                        8 -> if (param1 == param2) 1 else 0
                        else -> error("Unknown opcode $opcode")
                    }
                    setParam(modes[2], 3, result)
                    ip += 4
                }
                3 -> {
                    if (inputs.isEmpty()) {
                        needsInput = true
                        return
                    }
                    needsInput = false
                    setParam(modes[0], 1, inputs.removeAt(0))
                    ip += 2
                }
                4 -> {
                    outputs.add(getParam(modes[0], 1))
                    ip += 2
                    if (outputs.size == 3) return
                }
                5, 6 -> {
                    val param1 = getParam(modes[0], 1)
                    val param2 = getParam(modes[1], 2)
                    ip = if ((opcode == 5 && param1 != 0L) || (opcode == 6 && param1 == 0L)) param2 else ip + 3
                }
                9 -> {
                    relativeBase += getParam(modes[0], 1)
                    ip += 2
                }
                else -> error("Unknown opcode $opcode")
            }
        }
    }
}
