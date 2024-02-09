import java.io.File

fun main() {
    val input = File("input.txt").readText().split(",").map { it.toInt() }
    val target = input.sorted()[input.size / 2]
    val fuel = input.map { Math.abs(it - target) }.sum()
    println(fuel)
}