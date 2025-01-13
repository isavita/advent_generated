
import java.io.File

data class Position(val i: Int, val j: Int)

fun findPosition(mat: List<String>, ch: Char): Position {
    for (i in mat.indices) {
        for (j in mat[i].indices) {
            if (mat[i][j] == ch) {
                return Position(i, j)
            }
        }
    }
    return Position(-1, -1)
}

fun ok(mat: List<String>, st: Position, seq: String): Boolean {
    var curr = st
    for (i in seq.indices) {
        if (mat[curr.i][curr.j] == ' ') {
            return false
        }
        when (seq[i]) {
            '^' -> curr = curr.copy(i = curr.i - 1)
            'v' -> curr = curr.copy(i = curr.i + 1)
            '<' -> curr = curr.copy(j = curr.j - 1)
            '>' -> curr = curr.copy(j = curr.j + 1)
        }
        if (curr.i < 0 || curr.i >= mat.size || curr.j < 0 || curr.j >= mat[0].length) {
            return false
        }
    }
    return true
}

fun generateMoves(position: Position, objective: Char, pad: List<String>): String {
    val objPos = findPosition(pad, objective)
    val ret = StringBuilder()
    if (position.j > objPos.j) {
        ret.append("<".repeat(position.j - objPos.j))
    }
    if (position.i > objPos.i) {
        ret.append("^".repeat(position.i - objPos.i))
    }
    if (position.i < objPos.i) {
        ret.append("v".repeat(objPos.i - position.i))
    }
    if (position.j < objPos.j) {
        ret.append(">".repeat(objPos.j - position.j))
    }
    if (!ok(pad, position, ret.toString())) {
        ret.clear()
        if (position.j < objPos.j) {
            ret.append(">".repeat(objPos.j - position.j))
        }
        if (position.i > objPos.i) {
            ret.append("^".repeat(position.i - objPos.i))
        }
        if (position.i < objPos.i) {
            ret.append("v".repeat(objPos.i - position.i))
        }
        if (position.j > objPos.j) {
            ret.append("<".repeat(position.j - objPos.j))
        }
    }
    return ret.toString()
}

fun solve(code: String, robots: Int, keyPad: List<String>, robotPad: List<String>, maxRobots: Int, memo: MutableMap<Triple<String, Int, Position>, Int>): Int {
    if (robots <= 0) {
        return code.length
    }
    var posi = 3
    var posj = 2
    if (robots != maxRobots) {
        posi = 0
    }
    val key = Triple(code, robots, Position(posi, posj))
    if (memo.containsKey(key)) {
        return memo[key]!!
    }
    var ret = 0
    for (i in code.indices) {
        val ch = code[i]
        val moves: String
        if (robots == maxRobots) {
            moves = generateMoves(Position(posi, posj), ch, keyPad)
            val pos = findPosition(keyPad, ch)
            posi = pos.i
            posj = pos.j
        } else {
            moves = generateMoves(Position(posi, posj), ch, robotPad)
            val pos = findPosition(robotPad, ch)
            posi = pos.i
            posj = pos.j
        }
        ret += solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots, memo)
    }
    memo[key] = ret
    return ret
}

fun main() {
    val content = File("input.txt").readText().trim()
    val maxRobots = 3
    val keyPad = listOf("789", "456", "123", " 0A")
    val robotPad = listOf(" ^A", "<v>")
    var ret = 0
    val codes = content.split("\n")
    val memo = mutableMapOf<Triple<String, Int, Position>, Int>()
    for (code in codes) {
        val trimmedCode = code.trim()
        if (trimmedCode.isEmpty()) {
            continue
        }
        var numericPart = 0
        for (i in trimmedCode.indices) {
            if (trimmedCode[i] in '0'..'9') {
                numericPart = numericPart * 10 + (trimmedCode[i] - '0')
            }
        }
        val sv = solve(trimmedCode, maxRobots, keyPad, robotPad, maxRobots, memo)
        ret += sv * numericPart
    }
    println(ret)
}
