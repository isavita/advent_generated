import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText()

    var score = 0
    var depth = 0
    var inGarbage = false
    var cancelNext = false
    var garbageCount = 0

    for (ch in input) {
        if (cancelNext) {
            cancelNext = false
            continue
        }

        if (inGarbage) {
            when (ch) {
                '!' -> cancelNext = true
                '>' -> inGarbage = false
                else -> garbageCount++
            }
        } else {
            when (ch) {
                '{' -> depth++
                '}' -> {
                    score += depth
                    depth--
                }
                '<' -> inGarbage = true
            }
        }
    }

    println(garbageCount)
}