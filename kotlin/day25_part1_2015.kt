import java.io.File
import java.util.regex.Pattern
import kotlin.math.floor

fun main() {
    val data = File("input.txt").readText()
    val re = Pattern.compile("row (\\d+), column (\\d+)")
    val matches = re.matcher(data)
    if (!matches.find()) {
        System.err.println("Invalid input format.")
        return
    }

    val row = matches.group(1).toInt()
    val column = matches.group(2).toInt()

    val pos = getPosition(row, column)
    val code = getCode(pos)

    println(code)
}

fun getPosition(row: Int, column: Int): Int {
    return (row + column - 2) * (row + column - 1) / 2 + column
}

fun getCode(position: Int): Int {
    val startCode = 20151125L
    val multiplier = 252533L
    val modulus = 33554393L

    var code = startCode
    for (i in 1 until position) {
        code = (code * multiplier) % modulus
    }
    return code.toInt()
}