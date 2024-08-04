import java.io.File

fun main() {
    val input = File("input.txt").readText().trim()
    val result = solve(input)
    println(result)
}

fun solve(input: String): Long {
    val positions = parseInput(input)
    val (w1, w2) = play(intArrayOf(positions[0], positions[1]), intArrayOf(0, 0), 3, true, mutableMapOf())
    return maxOf(w1, w2)
}

fun play(positions: IntArray, scores: IntArray, rollsLeftInTurn: Int, isPlayer1sTurn: Boolean, memo: MutableMap<String, LongArray>): LongArray {
    val key = "${positions.joinToString(",")}-${scores.joinToString(",")}-rollsLeft:$rollsLeftInTurn-isPlayer1:$isPlayer1sTurn"
    if (memo.containsKey(key)) return memo[key]!!

    val playerIndex = if (isPlayer1sTurn) 0 else 1
    val scoresCopy = scores.clone()

    if (rollsLeftInTurn == 0) {
        scoresCopy[playerIndex] += positions[playerIndex]
        if (scoresCopy[playerIndex] >= 21) return if (playerIndex == 0) longArrayOf(1, 0) else longArrayOf(0, 1)

        return play(positions, scoresCopy, 3, !isPlayer1sTurn, memo)
    }

    var wins = LongArray(2)
    for (roll in 1..3) {
        val positionsCopy = positions.clone()
        positionsCopy[playerIndex] = (positionsCopy[playerIndex] + roll - 1) % 10 + 1
        val (r1, r2) = play(positionsCopy, scoresCopy, rollsLeftInTurn - 1, isPlayer1sTurn, memo)
        wins[0] += r1
        wins[1] += r2
    }

    memo[key] = wins
    return wins
}

fun parseInput(input: String): IntArray {
    return input.lines().map { it.split(": ").last().toInt() }.toIntArray()
}