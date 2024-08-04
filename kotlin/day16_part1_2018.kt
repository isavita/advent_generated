import java.io.File

data class Sample(val before: List<Int>, val instruction: List<Int>, val after: List<Int>)

fun main() {
    val samples = readSamples("input.txt")
    val result = countSamplesBehavingLikeThreeOrMoreOpcodes(samples)
    println(result)
}

fun readSamples(fileName: String): List<Sample> {
    val lines = File(fileName).readLines()
    val samples = mutableListOf<Sample>()
    var i = 0
    while (i < lines.size) {
        if (lines[i].startsWith("Before:")) {
            val before = lines[i].removePrefix("Before: ").removeSuffix("]").removePrefix("[").split(", ").map { it.toInt() }
            val instruction = lines[i + 1].split(" ").map { it.toInt() }
            val after = lines[i + 2].removePrefix("After:  ").removeSuffix("]").removePrefix("[").split(", ").map { it.toInt() }
            samples.add(Sample(before, instruction, after))
            i += 4 // Skip to the next sample
        } else {
            i += 1 // Move to the next line if it doesn't start with "Before:"
        }
    }
    return samples
}

fun countSamplesBehavingLikeThreeOrMoreOpcodes(samples: List<Sample>): Int {
    return samples.count { sample ->
        val possibleOpcodes = opcodes.count { opcode ->
            val registers = sample.before.toMutableList()
            opcode(registers, sample.instruction[1], sample.instruction[2], sample.instruction[3])
            registers == sample.after
        }
        possibleOpcodes >= 3
    }
}

val opcodes = listOf(
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] + r[b] }, // addr
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] + b }, // addi
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] * r[b] }, // mulr
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] * b }, // muli
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] and r[b] }, // banr
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] and b }, // bani
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] or r[b] }, // borr
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] or b }, // bori
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = r[a] }, // setr
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = a }, // seti
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = if (a > r[b]) 1 else 0 }, // gtir
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = if (r[a] > b) 1 else 0 }, // gtri
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = if (r[a] > r[b]) 1 else 0 }, // gtrr
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = if (a == r[b]) 1 else 0 }, // eqir
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = if (r[a] == b) 1 else 0 }, // eqri
    { r: MutableList<Int>, a: Int, b: Int, c: Int -> r[c] = if (r[a] == r[b]) 1 else 0 } // eqrr
)