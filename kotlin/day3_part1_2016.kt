import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val possibleTriangles = input.count { isPossibleTriangle(it) }
    println(possibleTriangles)
}

fun isPossibleTriangle(line: String): Boolean {
    val sides = line.split("\\s+".toRegex()).filter { it.isNotBlank() }.map { it.toInt() }.sorted()
    return sides.size == 3 && sides[0] + sides[1] > sides[2]
}