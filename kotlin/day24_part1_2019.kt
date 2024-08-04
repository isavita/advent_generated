import java.io.File

fun main() {
    val inputFileName = "input.txt"
    val initialState = readInput(inputFileName)
    val layoutsSeen = mutableSetOf<String>()
    var currentState = initialState

    while (true) {
        val stateString = stateToString(currentState)
        if (stateString in layoutsSeen) {
            println("Biodiversity rating: ${calculateBiodiversityRating(currentState)}")
            break
        }
        layoutsSeen.add(stateString)
        currentState = nextState(currentState)
    }
}

fun readInput(fileName: String): List<String> {
    return File(fileName).readLines()
}

fun stateToString(state: List<String>): String {
    return state.joinToString("")
}

fun nextState(currentState: List<String>): List<String> {
    val newState = currentState.map { it.toCharArray() }
    for (i in currentState.indices) {
        for (j in currentState[i].indices) {
            val adjacentBugs = countAdjacentBugs(currentState, i, j)
            if (currentState[i][j] == '#') {
                if (adjacentBugs != 1) {
                    newState[i][j] = '.'
                }
            } else {
                if (adjacentBugs == 1 || adjacentBugs == 2) {
                    newState[i][j] = '#'
                }
            }
        }
    }
    return newState.map { it.joinToString("") }
}

fun countAdjacentBugs(state: List<String>, row: Int, col: Int): Int {
    val directions = listOf(
        Pair(-1, 0), Pair(1, 0), Pair(0, -1), Pair(0, 1)
    )
    var count = 0
    for ((dr, dc) in directions) {
        val newRow = row + dr
        val newCol = col + dc
        if (newRow in state.indices && newCol in state[newRow].indices && state[newRow][newCol] == '#') {
            count++
        }
    }
    return count
}

fun calculateBiodiversityRating(state: List<String>): Int {
    var rating = 0
    var power = 0
    for (i in state.indices) {
        for (j in state[i].indices) {
            if (state[i][j] == '#') {
                rating += (1 shl power)
            }
            power++
        }
    }
    return rating
}