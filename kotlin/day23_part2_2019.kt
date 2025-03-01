
import java.io.File
import java.util.concurrent.LinkedBlockingQueue

class IntcodeComputer(private val program: List<Long>, private val address: Int) {
    private val inputQueue = LinkedBlockingQueue<Long>()
    private val outputQueue = LinkedBlockingQueue<Long>()
    private var ip = 0
    private var relativeBase = 0
    private val memory = program.toMutableList().apply {
        repeat(10000) { add(0) } // Expand memory
    }
    var isIdle = false

    init {
        inputQueue.offer(address.toLong())
    }

    fun run(): Boolean {
        while (true) {
            val instruction = memory[ip].toInt()
            val opcode = instruction % 100
            val modes = (instruction / 100).toString().padStart(3, '0').map { it.toString().toInt() }.reversed()

            fun getParam(offset: Int): Long {
                return when (modes[offset - 1]) {
                    0 -> memory[memory[ip + offset].toInt()].toLong()
                    1 -> memory[ip + offset]
                    2 -> memory[memory[ip + offset].toInt() + relativeBase].toLong()
                    else -> error("Invalid parameter mode")
                }
            }

            fun setParam(offset: Int, value: Long) {
                when (modes[offset - 1]) {
                    0 -> memory[memory[ip + offset].toInt()] = value
                    2 -> memory[memory[ip + offset].toInt() + relativeBase] = value
                    else -> error("Invalid parameter mode for set")
                }
            }

            when (opcode) {
                1 -> { // Add
                    setParam(3, getParam(1) + getParam(2))
                    ip += 4
                }

                2 -> { // Multiply
                    setParam(3, getParam(1) * getParam(2))
                    ip += 4
                }

                3 -> { // Input
                    val input = inputQueue.poll()
                    if (input == null) {
                        isIdle = true
                        inputQueue.offer(-1)
                        return false
                    } else{
                        isIdle = false;
                    }

                    setParam(1, input)
                    ip += 2
                }

                4 -> { // Output
                    outputQueue.offer(getParam(1))
                    ip += 2
                }

                5 -> { // Jump-if-true
                    if (getParam(1) != 0L) {
                        ip = getParam(2).toInt()
                    } else {
                        ip += 3
                    }
                }

                6 -> { // Jump-if-false
                    if (getParam(1) == 0L) {
                        ip = getParam(2).toInt()
                    } else {
                        ip += 3
                    }
                }

                7 -> { // Less than
                    setParam(3, if (getParam(1) < getParam(2)) 1 else 0)
                    ip += 4
                }

                8 -> { // Equals
                    setParam(3, if (getParam(1) == getParam(2)) 1 else 0)
                    ip += 4
                }

                9 -> { // Adjust relative base
                    relativeBase += getParam(1).toInt()
                    ip += 2
                }

                99 -> return true // Halt
                else -> error("Invalid opcode: $opcode")
            }
        }
    }

    fun receivePacket(x: Long, y: Long) {
        inputQueue.offer(x)
        inputQueue.offer(y)
    }

    fun getOutput(): Long? {
        return outputQueue.poll()
    }
}

fun main() {
    val program = File("input.txt").readText().trim().split(",").map { it.toLong() }

    val computers = (0..49).map { IntcodeComputer(program, it) }
    var natPacket: Pair<Long, Long>? = null
    val deliveredYValues = mutableSetOf<Long>()
    var lastDeliveredY: Long? = null

    while (true) {
        var allIdle = true

        for (computer in computers) {
            if (!computer.run()) {
                 //Computer is not halted and still running
            }
           
            while (true) {
                val address = computer.getOutput() ?: break
                val x = computer.getOutput() ?: error("Expected X value")
                val y = computer.getOutput() ?: error("Expected Y value")
                allIdle = false

                if (address == 255L) {
                    natPacket = x to y
                } else {
                    computers[address.toInt()].receivePacket(x, y)
                }
            }
            if(!computer.isIdle){
                allIdle = false;
            }
        }

        if (allIdle && natPacket != null) {
            computers[0].receivePacket(natPacket.first, natPacket.second)
            if (natPacket.second == lastDeliveredY) {
                println(natPacket.second)
                return
            }
            lastDeliveredY = natPacket.second
            natPacket = null // Clear NAT packet after sending
        }
    }
}
