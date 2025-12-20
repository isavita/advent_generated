import java.io.File
import kotlin.math.abs
import kotlin.math.round

private const val INF = Int.MAX_VALUE

fun parseLine(line: String): Pair<List<IntArray>, IntArray> {
    val buttons = mutableListOf<IntArray>()
    val targets = mutableListOf<Int>()
    var i = 0
    while (i < line.length) {
        when (line[i]) {
            '(' -> {
                i++
                val list = mutableListOf<Int>()
                while (i < line.length && line[i] != ')') {
                    var num = 0
                    while (i < line.length && line[i].isDigit()) {
                        num = num * 10 + (line[i] - '0')
                        i++
                    }
                    list.add(num)
                    if (i < line.length && line[i] == ',') i++
                }
                if (i < line.length && line[i] == ')') i++
                buttons.add(list.toIntArray())
            }
            '{' -> {
                i++
                while (i < line.length && line[i] != '}') {
                    var num = 0
                    while (i < line.length && line[i].isDigit()) {
                        num = num * 10 + (line[i] - '0')
                        i++
                    }
                    targets.add(num)
                    if (i < line.length && line[i] == ',') i++
                }
                break
            }
            else -> i++
        }
    }
    return Pair(buttons, targets.toIntArray())
}

private class Solver(
    private val buttons: List<IntArray>,
    private val targets: IntArray
) {
    private val numCounters = targets.size
    private val numButtons = buttons.size
    private val matrix = Array(numCounters) { DoubleArray(numButtons + 1) }
    private val pivotCol = IntArray(numCounters) { -1 }
    private val isPivot = BooleanArray(numButtons)
    private val pivotRows = IntArray(numButtons) { -1 }
    private val freeVars = IntArray(numButtons)
    private var numFree = 0
    private val maxPresses = IntArray(numButtons)
    private val freeValues = IntArray(numButtons)
    private var bestResult = INF

    private fun gauss() {
        for (r in 0 until numCounters) {
            for (c in 0..numButtons) matrix[r][c] = 0.0
            matrix[r][numButtons] = targets[r].toDouble()
        }
        for (b in 0 until numButtons) {
            for (cIdx in buttons[b]) {
                if (cIdx < numCounters) matrix[cIdx][b] = 1.0
            }
        }
        var row = 0
        for (col in 0 until numButtons) {
            if (row >= numCounters) break
            var maxRow = row
            for (r in row + 1 until numCounters) {
                if (abs(matrix[r][col]) > abs(matrix[maxRow][col])) maxRow = r
            }
            if (abs(matrix[maxRow][col]) < 1e-9) continue
            val tmp = matrix[row]
            matrix[row] = matrix[maxRow]
            matrix[maxRow] = tmp
            val scale = matrix[row][col]
            for (c in col..numButtons) matrix[row][c] /= scale
            for (r in 0 until numCounters) {
                if (r != row && abs(matrix[r][col]) > 1e-9) {
                    val factor = matrix[r][col]
                    for (c in col..numButtons) matrix[r][c] -= factor * matrix[row][c]
                }
            }
            pivotCol[row] = col
            row++
        }
        val rank = row
        for (i in 0 until numButtons) {
            isPivot[i] = false
            pivotRows[i] = -1
        }
        for (r in 0 until rank) {
            val c = pivotCol[r]
            if (c >= 0) {
                isPivot[c] = true
                pivotRows[c] = r
            }
        }
        numFree = 0
        for (i in 0 until numButtons) if (!isPivot[i]) {
            freeVars[numFree++] = i
        }
        for (i in 0 until numButtons) {
            var limit = INF
            for (cIdx in buttons[i]) {
                if (cIdx < numCounters && targets[cIdx] < limit) limit = targets[cIdx]
            }
            maxPresses[i] = if (limit == INF) 0 else limit
        }
        for (i in 0 until numFree) {
            for (j in i + 1 until numFree) {
                if (maxPresses[freeVars[i]] > maxPresses[freeVars[j]]) {
                    val t = freeVars[i]
                    freeVars[i] = freeVars[j]
                    freeVars[j] = t
                }
            }
        }
    }

    private fun computePivots(): Int {
        val presses = IntArray(numButtons)
        for (i in 0 until numFree) presses[freeVars[i]] = freeValues[i]
        for (r in numCounters - 1 downTo 0) {
            val col = pivotCol[r]
            if (col < 0) continue
            var v = matrix[r][numButtons]
            for (c in col + 1 until numButtons) v -= matrix[r][c] * presses[c]
            val intVal = round(v).toInt()
            if (abs(v - intVal) > 1e-6) return 0
            if (intVal < 0 || intVal > maxPresses[col]) return 0
            presses[col] = intVal
        }
        var sum = 0
        for (p in presses) sum += p
        return sum
    }

    private fun enumerate(idx: Int, curSum: Int) {
        if (curSum >= bestResult) return
        if (idx == numFree) {
            val sum = computePivots()
            if (sum > 0 && sum < bestResult) bestResult = sum
            return
        }
        val fv = freeVars[idx]
        val maxVal = maxPresses[fv]
        for (v in 0..maxVal) {
            freeValues[idx] = v
            enumerate(idx + 1, curSum + v)
        }
    }

    fun solve(): Int {
        gauss()
        bestResult = INF
        enumerate(0, 0)
        return if (bestResult == INF) -1 else bestResult
    }
}

fun main() {
    val file = File("input.txt")
    if (!file.exists()) return
    var total = 0
    file.forEachLine { raw ->
        val line = raw.trim()
        if (line.isEmpty()) return@forEachLine
        val (buttons, targets) = parseLine(line)
        if (buttons.isEmpty() || targets.isEmpty()) return@forEachLine
        val res = Solver(buttons, targets).solve()
        if (res > 0) total += res
    }
    println(total)
}
