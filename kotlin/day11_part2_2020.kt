import java.io.File

fun main(args: Array<String>) {
    val seatingArea = mutableListOf<CharArray>()
    File("input.txt").forEachLine { seatingArea.add(it.toCharArray()) }

    var stabilized = false
    while (!stabilized) {
        val result = simulateSeatingPartTwo(seatingArea)
        seatingArea.clear()
        seatingArea.addAll(result.first)
        stabilized = result.second
    }

    println(countOccupiedSeats(seatingArea))
}

fun simulateSeatingPartTwo(seatingArea: List<CharArray>): Pair<List<CharArray>, Boolean> {
    val rows = seatingArea.size
    val cols = seatingArea[0].size
    val newSeatingArea = Array(rows) { CharArray(cols) }
    for (i in seatingArea.indices) {
        newSeatingArea[i] = seatingArea[i].copyOf()
    }
    var stabilized = true

    for (i in seatingArea.indices) {
        for (j in seatingArea[i].indices) {
            when (seatingArea[i][j]) {
                'L' -> if (countVisibleOccupied(seatingArea, i, j) == 0) {
                    newSeatingArea[i][j] = '#'
                    stabilized = false
                }
                '#' -> if (countVisibleOccupied(seatingArea, i, j) >= 5) {
                    newSeatingArea[i][j] = 'L'
                    stabilized = false
                }
            }
        }
    }

    return Pair(newSeatingArea.toList(), stabilized)
}

fun countVisibleOccupied(seatingArea: List<CharArray>, row: Int, col: Int): Int {
    var count = 0
    val directions = listOf(-1 to -1, 0 to -1, 1 to -1, -1 to 0, 1 to 0, -1 to 1, 0 to 1, 1 to 1)
    for (dir in directions) {
        var (r, c) = row + dir.first to col + dir.second
        while (r in seatingArea.indices && c in seatingArea[r].indices) {
            if (seatingArea[r][c] == 'L') {
                break
            }
            if (seatingArea[r][c] == '#') {
                count++
                break
            }
            r += dir.first
            c += dir.second
        }
    }
    return count
}

fun countOccupiedSeats(seatingArea: List<CharArray>): Int {
    var count = 0
    for (row in seatingArea) {
        for (seat in row) {
            if (seat == '#') {
                count++
            }
        }
    }
    return count
}