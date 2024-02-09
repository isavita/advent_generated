import java.io.File

fun main(args: Array<String>) {
    var prev = 0
    var current = 0
    var count = 0

    File("input.txt").forEachLine {
        current = it.toInt()
        if (prev != 0 && current > prev) {
            count++
        }
        prev = current
    }

    println(count)
}