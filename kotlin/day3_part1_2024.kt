
import java.io.File
import java.util.regex.Pattern

fun main() {
    val input = File("input.txt").readText()
    val pattern = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)")
    val matcher = pattern.matcher(input)
    var totalSum = 0
    while (matcher.find()) {
        val x = matcher.group(1).toInt()
        val y = matcher.group(2).toInt()
        totalSum += x * y
    }
    println(totalSum)
}
