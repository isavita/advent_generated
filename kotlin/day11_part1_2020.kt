import java.io.File

fun main() {
    // Read input from file
    val input = File("input.txt").readLines()

    // Convert input to 2D list
    var seats = input.map { it.toCharArray().toList() }

    // Function to count adjacent occupied seats
    fun countAdjacentOccupied(row: Int, col: Int): Int {
        val directions = listOf(
            Pair(-1, -1), Pair(-1, 0), Pair(-1, 1),
            Pair(0, -1),             Pair(0, 1),
            Pair(1, -1), Pair(1, 0), Pair(1, 1)
        )
        var count = 0
        for (dir in directions) {
            val newRow = row + dir.first
            val newCol = col + dir.second
            if (newRow in 0 until seats.size && newCol in 0 until seats[0].size && seats[newRow][newCol] == '#') {
                count++
            }
        }
        return count
    }

    // Function to apply the rules and return the new state
    fun applyRules(): List<List<Char>> {
        val newSeats = seats.map { it.toMutableList() }.toMutableList()
        for (row in seats.indices) {
            for (col in seats[0].indices) {
                when (seats[row][col]) {
                    'L' -> if (countAdjacentOccupied(row, col) == 0) newSeats[row][col] = '#'
                    '#' -> if (countAdjacentOccupied(row, col) >= 4) newSeats[row][col] = 'L'
                }
            }
        }
        return newSeats
    }

    // Apply the rules repeatedly until no seats change state
    var newSeats = applyRules()
    while (newSeats != seats) {
        seats = newSeats
        newSeats = applyRules()
    }

    // Count the number of occupied seats
    val occupiedSeats = seats.sumBy { row -> row.count { it == '#' } }
    println(occupiedSeats)
}