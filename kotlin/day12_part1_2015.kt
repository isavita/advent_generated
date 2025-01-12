
import java.io.File

fun main() {
    val input = File("input.txt").readText()
    println(sumNumbersInJson(input))
}

fun sumNumbersInJson(json: String): Int {
    val numberRegex = Regex("-?\\d+")
    return numberRegex.findAll(json).sumOf { it.value.toInt() }
}
