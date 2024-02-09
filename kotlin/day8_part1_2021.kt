
import java.io.File

fun main(args: Array<String>) {
    val inputFile = File("input.txt")
    var count = 0

    inputFile.forEachLine {
        val parts = it.split(" | ")
        val output = parts[1]
        output.split(" ").forEach { digit ->
            when (digit.length) {
                2, 4, 3, 7 -> count++
            }
        }
    }

    println(count)
}
