import java.io.File

fun main() {
    val lines = File("input.txt").readLines().filter { it.isNotBlank() }
    val leftList = mutableListOf<Int>()
    val rightList = mutableListOf<Int>()

    for (line in lines) {
        val numbers = line.trim().split("\\s+".toRegex()).map { it.toInt() }
        leftList.add(numbers.first())
        rightList.add(numbers.last())
    }

    val totalDistance = leftList.sorted()
        .zip(rightList.sorted())
        .sumOf { (left, right) -> Math.abs(left - right) }

    println("Total distance: $totalDistance")
}
