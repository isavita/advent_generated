import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()

    val instructions = mapOf(
        "addr" to { r: IntArray, a: Int, b: Int -> r[a] + r[b] },
        "addi" to { r: IntArray, a: Int, b: Int -> r[a] + b },
        "mulr" to { r: IntArray, a: Int, b: Int -> r[a] * r[b] },
        "muli" to { r: IntArray, a: Int, b: Int -> r[a] * b },
        "banr" to { r: IntArray, a: Int, b: Int -> r[a] and r[b] },
        "bani" to { r: IntArray, a: Int, b: Int -> r[a] and b },
        "borr" to { r: IntArray, a: Int, b: Int -> r[a] or r[b] },
        "bori" to { r: IntArray, a: Int, b: Int -> r[a] or b },
        "setr" to { r: IntArray, a: Int, b: Int -> r[a] },
        "seti" to { r: IntArray, a: Int, b: Int -> a },
        "gtir" to { r: IntArray, a: Int, b: Int -> if (a > r[b]) 1 else 0 },
        "gtri" to { r: IntArray, a: Int, b: Int -> if (r[a] > b) 1 else 0 },
        "gtrr" to { r: IntArray, a: Int, b: Int -> if (r[a] > r[b]) 1 else 0 },
        "eqir" to { r: IntArray, a: Int, b: Int -> if (a == r[b]) 1 else 0 },
        "eqri" to { r: IntArray, a: Int, b: Int -> if (r[a] == b) 1 else 0 },
        "eqrr" to { r: IntArray, a: Int, b: Int -> if (r[a] == r[b]) 1 else 0 }
    )

    fun loadProgram(lines: List<String>): Pair<Int, List<(IntArray) -> Unit>> {
        var ipRegister = 0
        val program = mutableListOf<(IntArray) -> Unit>()
        val numPattern = Regex("\\d+")

        lines.forEach { line ->
            if (line.startsWith("#ip")) {
                ipRegister = line.split(" ")[1].toInt()
            } else {
                val parts = line.split(" ")
                val op = instructions[parts[0]]!!
                val nums = numPattern.findAll(line).map { it.value.toInt() }.toList()
                val (a, b, c) = Triple(nums[0], nums[1], nums[2])

                program.add { r ->
                    r[c] = op(r, a, b)
                }
            }
        }
        return Pair(ipRegister, program)
    }

    fun runProgram(ipRegister: Int, program: List<(IntArray) -> Unit>, registers: IntArray, maxCycles: Int): IntArray {
        var ip = 0
        var cycles = 0

        while (ip in program.indices) {
            registers[ipRegister] = ip
            program[ip](registers)
            ip = registers[ipRegister] + 1
            cycles++
            if (maxCycles > 0 && cycles >= maxCycles) {
                break
            }
        }
        return registers
    }

    fun max(slice: IntArray): Int {
        var maxValue = slice[0]
        for (v in slice) {
            if (v > maxValue) {
                maxValue = v
            }
        }
        return maxValue
    }

    val (ipRegister, program) = loadProgram(lines)

    val registers = IntArray(6) { 0 }
    registers[0] = 1
    runProgram(ipRegister, program, registers, 1000)
    val n = max(registers)
    var total = 0
    for (i in 1..n) {
        if (n % i == 0) {
            total += i
        }
    }
    println(total)
}