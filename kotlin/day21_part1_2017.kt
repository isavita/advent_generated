import java.io.File

fun main(args: Array<String>) {
    val rules = mutableMapOf<String, String>()

    File("input.txt").forEachLine {
        val parts = it.split(" => ")
        rules[parts[0]] = parts[1]
    }

    var grid = listOf(
        ".#.",
        "..#",
        "###"
    )

    repeat(5) {
        var newSize: Int
        var subSize: Int

        if (grid.size % 2 == 0) {
            subSize = 2
            newSize = grid.size / 2 * 3
        } else {
            subSize = 3
            newSize = grid.size / 3 * 4
        }

        val newGrid = Array(newSize) { "" }

        for (y in grid.indices step subSize) {
            for (x in grid.indices step subSize) {
                val square = mutableListOf<String>()
                for (dy in 0 until subSize) {
                    square.add(grid[y + dy].substring(x, x + subSize))
                }
                val newSquare = enhance(square.joinToString("/"), rules)
                newSquare.split("/").forEachIndexed { dy, row ->
                    newGrid[y / subSize * (subSize + 1) + dy] += row
                }
            }
        }
        grid = newGrid.toList()
    }

    var count = 0
    grid.forEach { row ->
        row.forEach { pixel ->
            if (pixel == '#') {
                count++
            }
        }
    }
    println(count)
}

fun enhance(input: String, rules: Map<String, String>): String {
    var tempInput = input
    repeat(4) {
        if (rules.containsKey(tempInput)) {
            return rules[tempInput]!!
        }
        tempInput = rotate(tempInput)
    }
    tempInput = flip(tempInput)
    repeat(4) {
        if (rules.containsKey(tempInput)) {
            return rules[tempInput]!!
        }
        tempInput = rotate(tempInput)
    }
    return ""
}

fun rotate(input: String): String {
    val parts = input.split("/")
    val size = parts.size
    val newParts = Array(size) { "" }
    for (x in 0 until size) {
        var newRow = ""
        for (y in size - 1 downTo 0) {
            newRow += parts[y][x]
        }
        newParts[x] = newRow
    }
    return newParts.joinToString("/")
}

fun flip(input: String): String {
    val parts = input.split("/")
    return parts.map { reverse(it) }.joinToString("/")
}

fun reverse(input: String): String {
    return input.reversed()
}