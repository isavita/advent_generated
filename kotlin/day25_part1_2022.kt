import java.io.File

fun snafuToDecimal(snafu: String): Long {
    var decimal = 0L
    for (char in snafu) {
        decimal *= 5
        decimal += when (char) {
            '2' -> 2
            '1' -> 1
            '0' -> 0
            '-' -> -1
            '=' -> -2
            else -> throw IllegalArgumentException("Invalid SNAFU character: $char")
        }
    }
    return decimal
}

fun decimalToSnafu(decimal: Long): String {
    var number = decimal
    val snafuBuilder = StringBuilder()

    while (number != 0L) {
        val remainder = number % 5
        number /= 5

        when (remainder) {
            0L -> snafuBuilder.append('0')
            1L -> snafuBuilder.append('1')
            2L -> snafuBuilder.append('2')
            3L -> {
                snafuBuilder.append('=')
                number++
            }
            4L -> {
                snafuBuilder.append('-')
                number++
            }
        }
    }

    return snafuBuilder.reverse().toString()
}

fun main() {
    val inputFile = File("input.txt")
    val snafuNumbers = inputFile.readLines()

    val totalDecimal = snafuNumbers.sumOf { snafuToDecimal(it) }
    val resultSnafu = decimalToSnafu(totalDecimal)

    println(resultSnafu)
}