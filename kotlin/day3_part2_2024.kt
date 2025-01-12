
import java.io.File
import java.util.regex.Pattern

fun main() {
    val input = File("input.txt").readText()
    val pattern = Pattern.compile("(mul\\((\\d{1,3}),(\\d{1,3})\\))|(do\\(\\))|(don't\\(\\))")
    val matcher = pattern.matcher(input)

    var enabled = true
    var totalSum = 0

    while (matcher.find()) {
        when {
            matcher.group(1) != null -> {
                if (enabled) {
                    val x = matcher.group(2).toInt()
                    val y = matcher.group(3).toInt()
                    totalSum += x * y
                }
            }
            matcher.group(4) != null -> enabled = true
            matcher.group(5) != null -> enabled = false
        }
    }
    println(totalSum)
}
