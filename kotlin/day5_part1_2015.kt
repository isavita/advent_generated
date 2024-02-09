import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    var nice = 0
    val disallowPattern = Regex("(ab|cd|pq|xy)")
    input.split("\n").forEach { line ->
        var vowels = 0
        line.forEach { char ->
            if ("aeiou".contains(char)) {
                vowels++
            }
        }
        var hasDouble = false
        for (i in 0 until line.length - 1) {
            if (line[i] == line[i + 1]) {
                hasDouble = true
                break
            }
        }
        if (vowels >= 3 && !disallowPattern.containsMatchIn(line) && hasDouble) {
            nice++
        }
    }

    println(nice)
}