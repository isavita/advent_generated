
import java.io.File
import java.util.StringTokenizer

private fun bits(x: Long) = java.lang.Long.bitCount(x)

private fun gaussianEliminationMinWeight(mat: Array<IntArray>, r: Int, c: Int): Int {
    val matrix = mat.map { it.clone() }.toTypedArray()
    val colIsPivot = BooleanArray(c)
    var pivotRow = 0
    for (col in 0 until c) {
        var sel = -1
        for (row in pivotRow until r) if (matrix[row][col] == 1) { sel = row; break }
        if (sel == -1) continue
        val tmp = matrix[pivotRow]
        matrix[pivotRow] = matrix[sel]
        matrix[sel] = tmp
        for (row in 0 until r) {
            if (row != pivotRow && matrix[row][col] == 1) {
                for (k in col..c) matrix[row][k] = matrix[row][k] xor matrix[pivotRow][k]
            }
        }
        colIsPivot[col] = true
        pivotRow++
    }
    for (row in pivotRow until r) if (matrix[row][c] == 1) return -1
    val freeVars = mutableListOf<Int>()
    for (col in 0 until c) if (!colIsPivot[col]) freeVars.add(col)
    val nFree = freeVars.size
    var minWeight = Int.MAX_VALUE
    val limit = 1L shl nFree
    for (mask in 0 until limit) {
        val x = IntArray(c)
        var cw = bits(mask).toInt()
        for (j in 0 until nFree) if ((mask ushr j) and 1L == 1L) x[freeVars[j]] = 1
        var curPivot = 0
        for (col in 0 until c) if (colIsPivot[col]) {
            var v = matrix[curPivot][c]
            for (k in col + 1 until c) if (matrix[curPivot][k] == 1) v = v xor x[k]
            x[col] = v
            if (v == 1) cw++
            curPivot++
        }
        if (cw < minWeight) minWeight = cw
    }
    return minWeight
}

fun main() {
    var total = 0
    File("input.txt").forEachLine { lineOrig ->
        var line = lineOrig
        val l = line.indexOf('[')
        val r = line.indexOf(']', l)
        if (l == -1 || r == -1) return@forEachLine
        val targetStr = line.substring(l + 1, r)
        val rows = targetStr.length
        val target = IntArray(rows) { if (targetStr[it] == '#') 1 else 0 }
        val buttonMatches = Regex("\\(([^)]*)\\)").findAll(line.substring(r + 1))
        val buttons = mutableListOf<IntArray>()
        for (m in buttonMatches) {
            val content = m.groupValues[1]
            if (content.isBlank()) {
                buttons.add(intArrayOf())
                continue
            }
            val toks = StringTokenizer(content, ",")
            val list = mutableListOf<Int>()
            while (toks.hasMoreTokens()) list.add(toks.nextToken().trim().toInt())
            buttons.add(list.toIntArray())
        }
        val cols = buttons.size
        val matrix = Array(rows) { IntArray(cols + 1) }
        for (i in 0 until rows) {
            for (j in 0 until cols) {
                var v = 0
                for (idx in buttons[j]) if (idx == i) { v = 1; break }
                matrix[i][j] = v
            }
            matrix[i][cols] = target[i]
        }
        val mw = gaussianEliminationMinWeight(matrix, rows, cols)
        if (mw != -1) total += mw
    }
    println(total)
}
