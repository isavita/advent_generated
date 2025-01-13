
import java.io.File
import java.util.*

val roomCoordToWantChar = mapOf(
    2 to 3 to 'A', 3 to 3 to 'A',
    2 to 5 to 'B', 3 to 5 to 'B',
    2 to 7 to 'C', 3 to 7 to 'C',
    2 to 9 to 'D', 3 to 9 to 'D'
)

fun main() {
    val input = File("input.txt").readText().trim()
    val ans = amphipod(input)
    println(ans)
}

fun amphipod(input: String): Int {
    val start = parseInput(input)
    val pq = PriorityQueue<State>(compareBy { it.energyUsed })
    pq.add(start)
    val seenGrids = mutableSetOf<String>()

    while (pq.isNotEmpty()) {
        val front = pq.remove()
        val key = front.grid.joinToString("") { it.joinToString("") }
        if (key in seenGrids) continue
        seenGrids.add(key)

        if (front.allDone()) return front.energyUsed

        val unsettledCoords = front.getUnsettledCoords()
        for (unsettledCoord in unsettledCoords) {
            val (ur, uc) = unsettledCoord
            val nextMoves = front.getNextPossibleMoves(unsettledCoord)
            for (nextCoord in nextMoves) {
                val (nr, nc) = nextCoord
                if (front.grid[nr][nc] != '.') error("should only be moving to walkable spaces, got ${front.grid[nr][nc]} at $nr,$nc")

                val cp = front.copy()
                cp.energyUsed += calcEnergy(cp.grid[ur][uc], unsettledCoord, nextCoord)
                cp.grid[nr][nc] = cp.grid[ur][uc]
                cp.grid[ur][uc] = '.'

                pq.add(cp)
            }
        }
    }

    error("should return from loop")
}

data class State(
    val grid: MutableList<MutableList<Char>>,
    var energyUsed: Int
) {
    fun allDone(): Boolean {
        for ((coord, want) in roomCoordToWantChar) {
            if (grid[coord.first][coord.second] != want) return false
        }
        return true
    }

    fun getUnsettledCoords(): List<Pair<Int, Int>> {
        val unsettled = mutableListOf<Pair<Int, Int>>()
        for (col in 1 until grid[0].size) {
            if (grid[1][col] in "ABCD") unsettled.add(1 to col)
        }
        for (col in listOf(3, 5, 7, 9)) {
            var roomFullFromBack = true
            for (row in grid.size - 2 downTo 2) {
                val coord = row to col
                val wantChar = roomCoordToWantChar[coord]!!
                val gotChar = grid[row][col]
                if (gotChar != '.') {
                    if (gotChar != wantChar) {
                        roomFullFromBack = false
                        unsettled.add(coord)
                    } else if (gotChar == wantChar && !roomFullFromBack) {
                        unsettled.add(coord)
                    }
                }
            }
        }
        return unsettled
    }

    fun getNextPossibleMoves(unsettledCoord: Pair<Int, Int>): List<Pair<Int, Int>> {
        val unsettledChar = grid[unsettledCoord.first][unsettledCoord.second]
        if (unsettledChar !in "ABCD") error("unexpected character to get next moves for $unsettledChar")

        val possible = mutableListOf<Pair<Int, Int>>()
        val startedInHallway = unsettledCoord.first == 1
        val q = LinkedList<Pair<Int, Int>>()
        q.add(unsettledCoord)
        val seen = mutableSetOf<Pair<Int, Int>>()

        while (q.isNotEmpty()) {
            val front = q.remove()
            if (front in seen) continue
            seen.add(front)

            if (front != unsettledCoord) {
                if (front !in coordsInFrontOfRooms) {
                    val wantChar = roomCoordToWantChar[front]
                    if (wantChar == null) {
                        if (!startedInHallway) possible.add(front)
                    } else if (wantChar == unsettledChar) {
                        var isStuckAmphipod = false
                        var roomHasDeeperOpenSpaces = false
                        for (r in front.first + 1 until grid.size - 1) {
                            val char = grid[r][front.second]
                            if (char == '.') roomHasDeeperOpenSpaces = true
                            if (char != '.' && char != unsettledChar) {
                                isStuckAmphipod = true
                                break
                            }
                        }
                        if (!roomHasDeeperOpenSpaces && !isStuckAmphipod) possible.add(front)
                    }
                }
            }

            for ((dr, dc) in listOf(-1 to 0, 1 to 0, 0 to -1, 0 to 1)) {
                val next = front.first + dr to front.second + dc
                if (grid[next.first][next.second] == '.') q.add(next)
            }
        }
        return possible
    }

    fun copy(): State {
        return State(grid.map { it.toMutableList() }.toMutableList(), energyUsed)
    }
}

val coordsInFrontOfRooms = setOf(1 to 3, 1 to 5, 1 to 7, 1 to 9)

fun parseInput(input: String): State {
    val grid = input.lines().map { it.toCharArray().toMutableList() }.toMutableList()
    return State(grid, 0)
}

fun calcEnergy(char: Char, start: Pair<Int, Int>, end: Pair<Int, Int>): Int {
    var dist = kotlin.math.abs(end.second - start.second)
    dist += start.first - 1
    dist += end.first - 1
    val energyPerType = mapOf('A' to 1, 'B' to 10, 'C' to 100, 'D' to 1000)
    if (char !in energyPerType) error("$char should not call calcEnergy()")
    return energyPerType[char]!! * dist
}
