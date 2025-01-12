
import java.io.File

fun main() {
    val (orderingRules, updates) = readInput("input.txt")
    val sum = updates.filter { isCorrectlyOrdered(it, orderingRules) }
        .sumOf { it[it.size / 2] }
    println(sum)
}

fun readInput(filename: String): Pair<List<Pair<Int, Int>>, List<List<Int>>> {
    val orderingRules = mutableListOf<Pair<Int, Int>>()
    val updates = mutableListOf<List<Int>>()
    var isUpdateSection = false

    File(filename).forEachLine { line ->
        val trimmedLine = line.trim()
        if (trimmedLine.isEmpty()) {
            isUpdateSection = true
            return@forEachLine
        }

        if (!isUpdateSection) {
            val parts = trimmedLine.split("|").map { it.trim() }
            if (parts.size == 2) {
                val x = parts[0].toIntOrNull()
                val y = parts[1].toIntOrNull()
                if (x != null && y != null) {
                    orderingRules.add(Pair(x, y))
                }
            }
        } else {
            val nums = trimmedLine.split(",").mapNotNull { it.trim().toIntOrNull() }
            if (nums.isNotEmpty()) {
                updates.add(nums)
            }
        }
    }
    return Pair(orderingRules, updates)
}

fun isCorrectlyOrdered(update: List<Int>, rules: List<Pair<Int, Int>>): Boolean {
    val position = update.withIndex().associate { it.value to it.index }
    return rules.all { (x, y) ->
        val posX = position[x]
        val posY = position[y]
        (posX == null || posY == null) || posX < posY
    }
}
