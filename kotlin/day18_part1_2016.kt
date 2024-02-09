import java.io.File

fun main(args: Array<String>) {
    val firstRow = readFirstRow("input.txt")
    val safeTilesCount = countSafeTiles(firstRow, 40)
    println(safeTilesCount)
}

fun readFirstRow(filename: String): String {
    val file = File(filename)
    return file.readLines().first()
}

fun countSafeTiles(firstRow: String, totalRows: Int): Int {
    var currentRow = firstRow
    var safeCount = currentRow.count { it == '.' }

    repeat(totalRows - 1) {
        var nextRow = ""
        for (j in currentRow.indices) {
            nextRow += if (isTrap(j - 1, j, j + 1, currentRow)) "^" else {
                safeCount++
                "."
            }
        }
        currentRow = nextRow
    }
    return safeCount
}

fun isTrap(left: Int, center: Int, right: Int, row: String): Boolean {
    val l = safeIfOutOfBounds(left, row)
    val c = row[center]
    val r = safeIfOutOfBounds(right, row)

    return (l == '^' && c == '^' && r == '.') ||
            (c == '^' && r == '^' && l == '.') ||
            (l == '^' && c == '.' && r == '.') ||
            (r == '^' && c == '.' && l == '.')
}

fun safeIfOutOfBounds(index: Int, row: String): Char {
    if (index < 0 || index >= row.length) {
        return '.'
    }
    return row[index]
}