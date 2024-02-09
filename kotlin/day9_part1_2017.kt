import java.io.File

fun main(args: Array<String>) {
    // Step 1: Read Input
    val file = File("input.txt")
    val lines = file.readLines()

    // Step 2: Initialize Variables
    var score = 0
    var depth = 0
    var inGarbage = false
    var cancelNext = false

    // Step 3: Process Stream
    for (line in lines) {
        for (ch in line) {
            if (cancelNext) {
                cancelNext = false
                continue
            }

            if (inGarbage) {
                if (ch == '!') {
                    cancelNext = true
                } else if (ch == '>') {
                    inGarbage = false
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
    }

    // Step 4: Print Score
    println(score)
}