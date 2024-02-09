import java.io.File

fun main() {
    val expenses = File("input.txt").readLines().map { it.toInt() }

    for (i in expenses.indices) {
        for (j in i + 1 until expenses.size) {
            if (expenses[i] + expenses[j] == 2020) {
                println(expenses[i] * expenses[j])
            }
            for (k in j + 1 until expenses.size) {
                if (expenses[i] + expenses[j] + expenses[k] == 2020) {
                    println(expenses[i] * expenses[j] * expenses[k])
                }
            }
        }
    }
}