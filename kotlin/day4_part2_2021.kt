import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    val result = solve(input)
    println(result)
}

fun solve(input: String): Int {
    val (nums, boards) = parseInput(input)

    var lastWinningScore = -1
    val alreadyWon = mutableMapOf<Int, Boolean>()
    for (n in nums) {
        for ((bi, b) in boards.withIndex()) {
            if (alreadyWon[bi] == true) {
                continue
            }
            val didWin = b.pickNum(n)
            if (didWin) {
                lastWinningScore = b.score() * n
                alreadyWon[bi] = true
            }
        }
    }

    return lastWinningScore
}

class BoardState(private val board: Array<IntArray>, private val picked: Array<BooleanArray>) {
    fun pickNum(num: Int): Boolean {
        for (r in board.indices) {
            for (c in board[0].indices) {
                if (board[r][c] == num) {
                    picked[r][c] = true
                }
            }
        }

        for (i in board.indices) {
            var isFullRow = true
            var isFullCol = true

            for (j in board.indices) {
                if (!picked[i][j]) {
                    isFullRow = false
                }

                if (!picked[j][i]) {
                    isFullCol = false
                }
            }
            if (isFullRow || isFullCol) {
                return true
            }
        }

        return false
    }

    fun score(): Int {
        var score = 0
        for (r in board.indices) {
            for (c in board[0].indices) {
                if (!picked[r][c]) {
                    score += board[r][c]
                }
            }
        }
        return score
    }
}

fun parseInput(input: String): Pair<List<Int>, List<BoardState>> {
    val lines = input.split("\n\n")

    val nums = lines[0].split(",").map { it.toInt() }

    val boards = mutableListOf<BoardState>()
    for (grid in lines.subList(1, lines.size)) {
        val b = mutableListOf<IntArray>()
        for (line in grid.split("\n")) {
            var cleanedLine = line.replace("  ", " ")
            while (cleanedLine[0] == ' ') {
                cleanedLine = cleanedLine.substring(1)
            }
            val parts = cleanedLine.split(" ")

            val row = parts.map { it.toInt() }.toIntArray()
            b.add(row)
        }

        boards.add(BoardState(b.toTypedArray(), Array(b.size) { BooleanArray(b[0].size) })
        )
    }

    return Pair(nums, boards)
}

fun toInt(s: String): Int {
    return s.toInt()
}