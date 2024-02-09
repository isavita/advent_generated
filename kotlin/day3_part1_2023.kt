import java.io.File

fun main(args: Array<String>) {
    val matrix = readFileToMatrix("input.txt")
    val sum = sumOfPartNumbers(matrix)
    println(sum)
}

fun readFileToMatrix(filePath: String): List<List<Char>> {
    val file = File(filePath)
    val matrix = mutableListOf<List<Char>>()
    file.forEachLine {
        matrix.add(it.toList())
    }
    return matrix
}

fun sumOfPartNumbers(matrix: List<List<Char>>): Int {
    var sum = 0
    val visited = Array(matrix.size) { BooleanArray(matrix[it].size) }

    matrix.forEachIndexed { y, row ->
        row.forEachIndexed { x, _ ->
            if (!visited[y][x] && Character.isDigit(matrix[y][x])) {
                val (number, length) = extractNumber(matrix, x, y)
                if (isAdjacentToSymbol(matrix, x, y, length)) {
                    sum += number
                }
                for (i in 0 until length) {
                    visited[y][x + i] = true
                }
            }
        }
    }
    return sum
}

fun extractNumber(matrix: List<List<Char>>, x: Int, y: Int): Pair<Int, Int> {
    var numberStr = ""
    var newX = x
    while (newX < matrix[y].size && Character.isDigit(matrix[y][newX])) {
        numberStr += matrix[y][newX]
        newX++
    }
    val number = numberStr.toInt()
    return Pair(number, numberStr.length)
}

fun isAdjacentToSymbol(matrix: List<List<Char>>, x: Int, y: Int, length: Int): Boolean {
    for (i in 0 until length) {
        if (checkAdjacent(matrix, x + i, y)) {
            return true
        }
    }
    return false
}

fun checkAdjacent(matrix: List<List<Char>>, x: Int, y: Int): Boolean {
    for (dy in -1..1) {
        for (dx in -1..1) {
            val adjX = x + dx
            val adjY = y + dy
            if (adjY in matrix.indices && adjX in matrix[adjY].indices) {
                if (!Character.isDigit(matrix[adjY][adjX]) && matrix[adjY][adjX] != '.') {
                    return true
                }
            }
        }
    }
    return false
}