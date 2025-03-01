
import java.io.File
import java.util.PriorityQueue

data class State(
    val grid: Array<CharArray>,
    val energyUsed: Int,
    val path: String
) : Comparable<State> {
    override fun compareTo(other: State): Int = energyUsed.compareTo(other.energyUsed)

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as State

        if (!grid.contentDeepEquals(other.grid)) return false
        if (energyUsed != other.energyUsed) return false
        if (path != other.path) return false

        return true
    }

    override fun hashCode(): Int {
        var result = grid.contentDeepHashCode()
        result = 31 * result + energyUsed
        result = 31 * result + path.hashCode()
        return result
    }
}

fun isAllDone(grid: Array<CharArray>, roomCoordToWantChar: Map<Pair<Int, Int>, Char>): Boolean {
    for ((coord, want) in roomCoordToWantChar) {
        if (grid[coord.first][coord.second] != want) {
            return false
        }
    }
    return true
}

fun getUnsettledCoords(grid: Array<CharArray>, roomCoordToWantChar: Map<Pair<Int, Int>, Char>): List<Pair<Int, Int>> {
    val unsettled = mutableListOf<Pair<Int, Int>>()
    for (col in 1 until grid[0].size) {
        if (grid[1][col] in "ABCD") {
            unsettled.add(1 to col)
        }
    }

    for (col in listOf(3, 5, 7, 9)) {
        var roomFullFromBack = true
        for (row in grid.size - 2 downTo 2) {
            val coord = row to col
            val wantChar = roomCoordToWantChar[coord]
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

fun getNextPossibleMoves(grid: Array<CharArray>, unsettledCoord: Pair<Int, Int>, roomCoordToWantChar: Map<Pair<Int, Int>, Char>): List<Pair<Int, Int>> {
    val unsettledChar = grid[unsettledCoord.first][unsettledCoord.second]
    if (unsettledChar !in "ABCD") {
        throw IllegalArgumentException("Unexpected character to get next moves for: $unsettledChar")
    }

    val possible = mutableListOf<Pair<Int, Int>>()
    val startedInHallway = unsettledCoord.first == 1

    val queue = mutableListOf(unsettledCoord)
    val seen = mutableSetOf<Pair<Int, Int>>()

    while (queue.isNotEmpty()) {
        val front = queue.removeAt(0)
        if (front in seen) {
            continue
        }
        seen.add(front)

        if (front != unsettledCoord) {
            if (front !in listOf(1 to 3, 1 to 5, 1 to 7, 1 to 9)) {
                val wantChar = roomCoordToWantChar[front]
                if (wantChar == null) {
                    if (!startedInHallway) {
                        possible.add(front)
                    }
                } else if (wantChar == unsettledChar) {
                    var isStuckAmphipod = false
                    var roomHasDeeperOpenSpaces = false
                    for (r in front.first + 1 until grid.size - 1) {
                        val char = grid[r][front.second]
                        if (char == '.') {
                            roomHasDeeperOpenSpaces = true
                        } else if (char != '.' && char != unsettledChar) {
                            isStuckAmphipod = true
                            break
                        }
                    }
                    if (!roomHasDeeperOpenSpaces && !isStuckAmphipod) {
                        possible.add(front)
                    }
                }
            }
        }

        for ((dr, dc) in listOf(-1 to 0, 1 to 0, 0 to -1, 0 to 1)) {
            val nextRow = front.first + dr
            val nextCol = front.second + dc
            if (nextRow in 0 until grid.size && nextCol in 0 until grid[0].size && grid[nextRow][nextCol] == '.') {
                queue.add(nextRow to nextCol)
            }
        }
    }

    return possible
}

fun calcEnergy(char: Char, start: Pair<Int, Int>, end: Pair<Int, Int>): Int {
    val dist = Math.abs(end.second - start.second) + (start.first - 1) + (end.first - 1)
    val energyPerType = mapOf('A' to 1, 'B' to 10, 'C' to 100, 'D' to 1000)
    return energyPerType[char]!! * dist
}

fun amphipod(inputStr: String): Int {
    val initialGrid = inputStr.lines().map { it.toCharArray() }.toTypedArray()

    val roomCoordToWantChar = mapOf(
        2 to 3 to 'A', 3 to 3 to 'A', 4 to 3 to 'A', 5 to 3 to 'A',
        2 to 5 to 'B', 3 to 5 to 'B', 4 to 5 to 'B', 5 to 5 to 'B',
        2 to 7 to 'C', 3 to 7 to 'C', 4 to 7 to 'C', 5 to 7 to 'C',
        2 to 9 to 'D', 3 to 9 to 'D', 4 to 9 to 'D', 5 to 9 to 'D'
    )

    val grid = initialGrid.toMutableList()
    grid.addAll(listOf(CharArray(0), CharArray(0)))
    grid[6] = initialGrid[4].clone()
    grid[5] = initialGrid[3].clone()
    grid[3] = "  #D#C#B#A#  ".toCharArray()
    grid[4] = "  #D#B#A#C#  ".toCharArray()

    val start = State(grid.toTypedArray(), 0, "")

    val minHeap = PriorityQueue<State>()
    minHeap.add(start)

    val seenGrids = mutableSetOf<String>()

    while (minHeap.isNotEmpty()) {
        val front = minHeap.poll()

        val key = front.grid.contentDeepToString()
        if (key in seenGrids) {
            continue
        }
        seenGrids.add(key)

        if (isAllDone(front.grid, roomCoordToWantChar)) {
            return front.energyUsed
        }

        val unsettledCoords = getUnsettledCoords(front.grid, roomCoordToWantChar)
        for (unsettledCoord in unsettledCoords) {
            val nextMoves = getNextPossibleMoves(front.grid, unsettledCoord, roomCoordToWantChar)
            for (nextCoord in nextMoves) {
                if (front.grid[nextCoord.first][nextCoord.second] != '.') {
                    throw IllegalArgumentException("Should only be moving to walkable spaces, got ${front.grid[nextCoord.first][nextCoord.second]} at $nextCoord")
                }

                val newGrid = front.grid.map { it.clone() }.toTypedArray()
                val energy = calcEnergy(newGrid[unsettledCoord.first][unsettledCoord.second], unsettledCoord, nextCoord)
                val newEnergyUsed = front.energyUsed + energy
                val newPath = front.path + "${newGrid[unsettledCoord.first][unsettledCoord.second]}$unsettledCoord->$nextCoord($newEnergyUsed),"

                newGrid[nextCoord.first][nextCoord.second] = newGrid[unsettledCoord.first][unsettledCoord.second]
                newGrid[unsettledCoord.first][unsettledCoord.second] = '.'

                minHeap.add(State(newGrid, newEnergyUsed, newPath))
            }
        }
    }

    throw IllegalStateException("Should return from loop")
}

fun main() {
    val inputStr = File("input.txt").readText().trim()
    println(amphipod(inputStr))
}
