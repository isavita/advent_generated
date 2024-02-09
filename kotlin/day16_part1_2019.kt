import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    val digits = input.map { it.toString().toInt() }.toMutableList()

    repeat(100) {
        digits.indices.forEach { i ->
            digits[i] = abs(digits.mapIndexed { j, it ->
                it * basePattern(((j + 1) / (i + 1)) % basePattern.size)
            }.sum() % 10)
        }
    }

    repeat(8) {
        print(digits[it])
    }
    println()
}

val basePattern = listOf(0, 1, 0, -1)

fun basePattern(index: Int): Int {
    return basePattern[index]
}

fun abs(x: Int): Int {
    return if (x < 0) -x else x
}