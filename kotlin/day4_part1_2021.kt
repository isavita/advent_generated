import java.io.File

data class Board(val numbers: Array<IntArray>, var marked: Array<BooleanArray>)

fun readInput(filename: String): Pair<List<Int>, List<Board>> {
    val lines = File(filename).readLines()
    val numbers = lines[0].split(",").map { it.toInt() }
    val boards = mutableListOf<Board>()

    for (i in 2 until lines.size step 6) {
        val boardNumbers = Array(5) { IntArray(5) }
        val marked = Array(5) { BooleanArray(5) }
        for (j in 0 until 5) {
            boardNumbers[j] = lines[i + j].trim().split(Regex("\\s+")).map { it.toInt() }.toIntArray()
        }
        boards.add(Board(boardNumbers, marked))
    }

    return Pair(numbers, boards)
}

fun markNumber(board: Board, number: Int): Boolean {
    for (i in 0 until 5) {
        for (j in 0 until 5) {
            if (board.numbers[i][j] == number) {
                board.marked[i][j] = true
                return checkWin(board)
            }
        }
    }
    return false
}

fun checkWin(board: Board): Boolean {
    // Check rows
    for (i in 0 until 5) {
        if (board.marked[i].all { it }) return true
    }
    // Check columns
    for (j in 0 until 5) {
        if ((0 until 5).all { board.marked[it][j] }) return true
    }
    return false
}

fun calculateScore(board: Board, lastNumber: Int): Int {
    var sumUnmarked = 0
    for (i in 0 until 5) {
        for (j in 0 until 5) {
            if (!board.marked[i][j]) {
                sumUnmarked += board.numbers[i][j]
            }
        }
    }
    return sumUnmarked * lastNumber
}

fun main() {
    val (numbers, boards) = readInput("input.txt")

    for (number in numbers) {
        for (board in boards) {
            if (markNumber(board, number)) {
                val score = calculateScore(board, number)
                println("Winning board score: $score")
                return
            }
        }
    }
}