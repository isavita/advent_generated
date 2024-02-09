import java.io.File

fun main(args: Array<String>) {
    val numbers = File("input.txt").readLines().map { it.toInt() }

    for (i in numbers.indices) {
        for (j in i + 1 until numbers.size) {
            if (numbers[i] + numbers[j] == 2020) {
                println(numbers[i] * numbers[j])
                return
            }
        }
    }
}