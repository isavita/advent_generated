import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val sum = file.readLines().sumBy { line ->
        val (firstDigit, lastDigit) = findFirstAndLastDigit(line)
        10 * firstDigit + lastDigit
    }
    
    println(sum)
}

fun findFirstAndLastDigit(line: String): Pair<Int, Int> {
    val digits = listOf("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

    var firstDigit = 0
    var lastDigit = 0
    line.forEachIndexed { index, char ->
        val digitStr = char.toString()
        if (digitStr in "0".."9") {
            if (firstDigit == 0) {
                firstDigit = digitStr.toInt()
            }
            lastDigit = digitStr.toInt()
        } else {
            digits.forEachIndexed { j, digit ->
                if (line.substring(index).startsWith(digit)) {
                    if (firstDigit == 0) {
                        firstDigit = j
                    }
                    lastDigit = j
                }
            }
        }
    }

    return Pair(firstDigit, lastDigit)
}