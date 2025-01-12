
import java.io.File
import java.util.*

fun main() {
    val (orderingRules, updates) = readInput("input.txt")
    var sum = 0
    for (update in updates) {
        if (!isCorrectlyOrdered(update, orderingRules)) {
            val sortedUpdate = sortUpdate(update, orderingRules)
            sum += sortedUpdate[sortedUpdate.size / 2]
        }
    }
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
            val parts = trimmedLine.split("|")
            if (parts.size == 2) {
                val x = parts[0].trim().toIntOrNull()
                val y = parts[1].trim().toIntOrNull()
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
    for ((x, y) in rules) {
        val posX = position[x]
        val posY = position[y]
        if (posX != null && posY != null && posX >= posY) {
            return false
        }
    }
    return true
}

fun sortUpdate(update: List<Int>, rules: List<Pair<Int, Int>>): List<Int> {
    val adjacency = mutableMapOf<Int, MutableList<Int>>()
    val pagesInUpdate = update.associateWith { true }.keys
    for (page in pagesInUpdate) {
        adjacency[page] = mutableListOf()
    }
    for ((x, y) in rules) {
        if (pagesInUpdate.contains(x) && pagesInUpdate.contains(y)) {
            adjacency[x]?.add(y)
        }
    }

    val visited = mutableSetOf<Int>()
    val tempMarked = mutableSetOf<Int>()
    val result = mutableListOf<Int>()

    fun visit(n: Int) {
        if (tempMarked.contains(n)) {
            throw IllegalStateException("Cycle detected")
        }
        if (!visited.contains(n)) {
            tempMarked.add(n)
            adjacency[n]?.forEach { visit(it) }
            tempMarked.remove(n)
            visited.add(n)
            result.add(n)
        }
    }

    for (page in pagesInUpdate) {
        if (!visited.contains(page)) {
            visit(page)
        }
    }
    result.reverse()
    return result
}
