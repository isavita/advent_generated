import java.io.File

fun main(args: Array<String>) {
    var sum = 0
    File("input.txt").forEachLine {
        if (it.isBlank()) return@forEachLine
        var firstDigit = -1
        var lastDigit = -1
        it.forEach {
            if (it.isDigit()) {
                if (firstDigit == -1) {
                    firstDigit = it.toString().toInt()
                }
                lastDigit = it.toString().toInt()
            }
        }
        if (firstDigit != -1 && lastDigit != -1) {
            val value = "$firstDigit$lastDigit".toInt()
            sum += value
        }
    }
    println(sum)
}