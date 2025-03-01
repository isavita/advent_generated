
import java.io.File

fun findPosition(mat: List<String>, ch: Char): Pair<Int, Int> {
    for (i in mat.indices) {
        for (j in mat[i].indices) {
            if (mat[i][j] == ch) {
                return Pair(i, j)
            }
        }
    }
    return Pair(-1, -1)
}

fun ok(mat: List<String>, st: Pair<Int, Int>, seq: String): Boolean {
    var (currI, currJ) = st
    for (ch in seq) {
        if (currI !in mat.indices || currJ !in mat[currI].indices || mat[currI][currJ] == ' ') {
            return false
        }
        when (ch) {
            '^' -> currI--
            'v' -> currI++
            '<' -> currJ--
            '>' -> currJ++
        }
    }
    return true
}

fun generateMoves(position: Pair<Int, Int>, objective: Char, pad: List<String>): String {
    val (objPosI, objPosJ) = findPosition(pad, objective)
    var (posI, posJ) = position
    var result = ""

    if (posJ > objPosJ) result += "<".repeat(posJ - objPosJ)
    if (posI > objPosI) result += "^".repeat(posI - objPosI)
    if (posI < objPosI) result += "v".repeat(objPosI - posI)
    if (posJ < objPosJ) result += ">".repeat(objPosJ - posJ)

    if (!ok(pad, position, result)) {
        result = ""
        if (posJ < objPosJ) result += ">".repeat(objPosJ - posJ)
        if (posI > objPosI) result += "^".repeat(posI - objPosI)
        if (posI < objPosI) result += "v".repeat(objPosI - posI)
        if (posJ > objPosJ) result += "<".repeat(posJ - objPosJ)
    }
    return result
}

fun solve(code: String, robots: Int, keyPad: List<String>, robotPad: List<String>, maxRobots: Int, memo: MutableMap<Triple<String, Int, Int>, Long>): Long {
    val key = Triple(code, robots, maxRobots)
    if (key in memo) {
        return memo[key]!!
    }

    if (robots <= 0) {
        return code.length.toLong()
    }

    var ret: Long = 0
    var posI = 3
    var posJ = 2
    if (robots != maxRobots) {
        posI = 0
    }

    for (ch in code) {
        val moves: String
        if (robots == maxRobots) {
            moves = generateMoves(Pair(posI, posJ), ch, keyPad)
            val (newPosI, newPosJ) = findPosition(keyPad, ch)
            posI = newPosI
            posJ = newPosJ
        } else {
            moves = generateMoves(Pair(posI, posJ), ch, robotPad)
            val (newPosI, newPosJ) = findPosition(robotPad, ch)
            posI = newPosI
            posJ = newPosJ
        }
        ret += solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots, memo)
    }

    memo[key] = ret
    return ret
}

fun main() {
    val content = File("input.txt").readText().trim()
    val maxRobots = 26
    val keyPad = listOf("789", "456", "123", " 0A")
    val robotPad = listOf(" ^A", "<v>")
    var ret: Long = 0
    val codes = content.split("\n")
    val memo = mutableMapOf<Triple<String, Int, Int>, Long>()

    for (code in codes) {
        val trimmedCode = code.trim()
        if (trimmedCode.isEmpty()) {
            continue
        }

        var numericPart = 0
        for (char in trimmedCode) {
            if (char in '0'..'9') {
                numericPart = numericPart * 10 + (char - '0')
            }
        }
        ret += solve(trimmedCode, maxRobots, keyPad, robotPad, maxRobots, memo) * numericPart.toLong()
    }
    println(ret)
}
