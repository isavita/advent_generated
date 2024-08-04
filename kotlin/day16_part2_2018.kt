import java.io.File
import java.util.regex.Pattern

data class OP(val a: Char, val b: Char, val action: Char, val name: String, var matchCount: MutableList<Byte> = mutableListOf())

fun main() {
    val input = File("input.txt").readText().trim().split("\n")

    val opcodes = listOf(
        OP('r', 'r', '+', "addr"),
        OP('r', 'v', '+', "addi"),
        OP('r', 'r', '*', "mulr"),
        OP('r', 'v', '*', "muli"),
        OP('r', 'r', '&', "banr"),
        OP('r', 'v', '&', "bani"),
        OP('r', 'r', '|', "borr"),
        OP('r', 'v', '|', "bori"),
        OP('r', 'r', 'a', "setr"),
        OP('v', 'r', 'a', "seti"),
        OP('v', 'r', '>', "gtir"),
        OP('r', 'v', '>', "gtri"),
        OP('r', 'r', '>', "gtrr"),
        OP('v', 'r', '=', "eqir"),
        OP('r', 'v', '=', "eqri"),
        OP('r', 'r', '=', "eqir")
    )

    var sum = 0
    var lineCount = 0
    while (lineCount < input.size) {
        if (input[lineCount].isNotEmpty() && input[lineCount][0] == 'B') {
            val split = regSplit(input[lineCount], "[^0-9]+")
            val registers = split.subList(1, 5).map { it.toInt() }
            val instruction = regSplit(input[lineCount + 1], "[^0-9]+").map { it.toByte() }
            val result = regSplit(input[lineCount + 2], "[^0-9]+").subList(1, 5).map { it.toInt() }
            val tempSum = testCode(registers, result, instruction, opcodes)

            if (tempSum >= 3) {
                sum++
            }

            lineCount += 4
        } else {
            break
        }
    }

    val orderedOpCodes = mutableMapOf<Byte, OP>()

    while (orderedOpCodes.size < 16) {
        for (i in opcodes.indices) {
            if (opcodes[i].matchCount.size == 1) {
                val c = opcodes[i].matchCount[0]
                orderedOpCodes[c] = opcodes[i]
                for (j in opcodes.indices) {
                    remove(opcodes[j], c)
                }
            }
        }
    }

    lineCount += 2

    var r = IntArray(4)

    while (lineCount < input.size) {
        val split = regSplit(input[lineCount], "[^0-9]+")
        val instruction = split.map { it.toByte() }
        r = runOp(orderedOpCodes[instruction[0]]!!, r, instruction)
        lineCount++
    }

    println(r[0])
}

fun remove(op: OP, c: Byte) {
    op.matchCount.remove(c)
}

fun add(op: OP, c: Byte) {
    if (c !in op.matchCount) {
        op.matchCount.add(c)
    }
}

fun testCode(registers: List<Int>, result: List<Int>, instruction: List<Byte>, opcodes: List<OP>): Int {
    var sum = 0
    for (i in opcodes.indices) {
        if (match(result, runOp(opcodes[i], registers.toIntArray(), instruction))) {
            add(opcodes[i], instruction[0])
            sum++
        }
    }
    return sum
}

fun match(r: List<Int>, c: IntArray): Boolean {
    return r.indices.all { r[it] == c[it] }
}

fun runOp(op: OP, registers: IntArray, instruction: List<Byte>): IntArray {
    val registerCP = registers.copyOf()
    var A: Int
    var B: Int
    if (op.a == 'r') {
        A = registerCP[instruction[1].toInt()]
    } else {
        A = instruction[1].toInt()
    }
    if (op.b == 'r') {
        B = registerCP[instruction[2].toInt()]
    } else {
        B = instruction[2].toInt()
    }
    registerCP[instruction[3].toInt()] = when (op.action) {
        '+' -> A + B
        '*' -> A * B
        '&' -> A and B
        '|' -> A or B
        'a' -> A
        '>' -> if (A > B) 1 else 0
        '=' -> if (A == B) 1 else 0
        else -> throw IllegalArgumentException("not valid instruction")
    }
    return registerCP
}

fun regSplit(text: String, delimiter: String): List<String> {
    val pattern = Pattern.compile(delimiter)
    val matcher = pattern.matcher(text)
    val result = mutableListOf<String>()
    var lastEnd = 0
    while (matcher.find()) {
        result.add(text.substring(lastEnd, matcher.start()))
        lastEnd = matcher.end()
    }
    result.add(text.substring(lastEnd))
    return result
}