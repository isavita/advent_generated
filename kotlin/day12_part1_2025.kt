
import java.io.File
import java.util.ArrayDeque
import kotlin.math.min

data class Pt(val r: Int, val c: Int) : Comparable<Pt> {
    override fun compareTo(other: Pt) = if (r == other.r) c - other.c else r - other.r
}
data class Piece(val pts: List<Pt>) {
    val n = pts.size
}

fun normalize(p: Piece): Piece {
    if (p.n == 0) return p
    var mr = p.pts[0].r
    var mc = p.pts[0].c
    for (pt in p.pts) {
        if (pt.r < mr) mr = pt.r
        if (pt.c < mc) mc = pt.c
    }
    val shifted = p.pts.map { Pt(it.r - mr, it.c - mc) }.sorted()
    return Piece(shifted)
}
fun rotate(p: Piece) = Piece(p.pts.map { Pt(it.c, -it.r) })
fun flip(p: Piece) = Piece(p.pts.map { Pt(it.r, -it.c) })

fun generateVariations(base: Piece): List<Piece> {
    val set = linkedSetOf<Piece>()
    var cur = base
    repeat(4) {
        val n = normalize(cur)
        if (set.add(n)) {}
        val f = normalize(flip(cur))
        if (set.add(f)) {}
        cur = rotate(cur)
    }
    return set.toList()
}

fun canPlace(rows: Int, cols: Int, grid: BooleanArray, p: Piece, r: Int, c: Int): Boolean {
    for (pt in p.pts) {
        val nr = r + pt.r
        val nc = c + pt.c
        if (nr !in 0 until rows || nc !in 0 until cols) return false
        if (grid[nr * cols + nc]) return false
    }
    return true
}
fun place(cols: Int, grid: BooleanArray, p: Piece, r: Int, c: Int, v: Boolean) {
    for (pt in p.pts) grid[(r + pt.r) * cols + (c + pt.c)] = v
}

fun checkIslands(rows: Int, cols: Int, grid: BooleanArray, cnt: IntArray, sz: Int, slackIdx: Int, shapes: Array<Piece>): Boolean {
    var minReal = Int.MAX_VALUE
    var any = false
    for (i in 0 until sz) if (i != slackIdx && cnt[i] > 0) {
        minReal = min(minReal, shapes[i].n)
        any = true
    }
    if (!any) return true
    var slack = cnt[slackIdx]
    val vis = BooleanArray(rows * cols)
    val q = IntArray(rows * cols)
    for (i in 0 until rows * cols) if (!grid[i] && !vis[i]) {
        var qs = 0
        var qe = 0
        q[qe++] = i
        vis[i] = true
        var size = 0
        while (qs < qe) {
            val cur = q[qs++]
            size++
            val r = cur / cols
            val c = cur % cols
            fun add(nr: Int, nc: Int) {
                val idx = nr * cols + nc
                if (!grid[idx] && !vis[idx]) {
                    vis[idx] = true
                    q[qe++] = idx
                }
            }
            if (r > 0) add(r - 1, c)
            if (r + 1 < rows) add(r + 1, c)
            if (c > 0) add(r, c - 1)
            if (c + 1 < cols) add(r, c + 1)
        }
        if (size < minReal) {
            if (slack >= size) slack -= size else return false
        }
    }
    return true
}

fun solveRec(
    rows: Int, cols: Int, grid: BooleanArray, cnt: IntArray, sz: Int,
    ids: IntArray, idc: Int, varr: Array<Array<Piece>>, varCnt: IntArray,
    slackIdx: Int, shapes: Array<Piece>
): Boolean {
    var empty = -1
    for (i in grid.indices) if (!grid[i]) { empty = i; break }
    if (empty == -1) return true
    val r = empty / cols
    val c = empty % cols
    if (!checkIslands(rows, cols, grid, cnt, sz, slackIdx, shapes)) return false
    for (i in 0 until idc) {
        val id = ids[i]
        if (cnt[id] == 0) continue
        cnt[id]--
        for (v in 0 until varCnt[id]) {
            val p = varr[id][v]
            if (canPlace(rows, cols, grid, p, r, c)) {
                place(cols, grid, p, r, c, true)
                if (solveRec(rows, cols, grid, cnt, sz, ids, idc, varr, varCnt, slackIdx, shapes)) return true
                place(cols, grid, p, r, c, false)
            }
        }
        cnt[id]++
    }
    return false
}

fun main() {
    val rawLines = File("input.txt").readLines()
    var maxId = -1
    for (ln in rawLines) {
        val s = ln.trim()
        if (s.isNotEmpty() && s.endsWith(":")) {
            val id = s.dropLast(1).toIntOrNull() ?: continue
            if (id > maxId) maxId = id
        }
    }
    val arrSize = maxId + 2
    val slackIdx = maxId + 1
    val shapes = Array(arrSize) { Piece(emptyList()) }
    var parsingShapes = true
    var curId = -1
    val curShape = mutableListOf<String>()
    val regionLines = mutableListOf<String>()
    for (ln in rawLines) {
        val s = ln.trim()
        if (s.isEmpty()) continue
        if (parsingShapes && s.contains('x') && s.contains(':')) parsingShapes = false
        if (parsingShapes) {
            if (s.endsWith(":")) {
                if (curId != -1 && curShape.isNotEmpty()) {
                    val pts = mutableListOf<Pt>()
                    for (r in curShape.indices) for (c in curShape[r].indices) if (curShape[r][c] == '#')
                        pts.add(Pt(r, c))
                    shapes[curId] = normalize(Piece(pts))
                    curShape.clear()
                }
                curId = s.dropLast(1).toInt()
            } else {
                curShape.add(s)
            }
        } else {
            regionLines.add(s)
        }
    }
    if (curId != -1 && curShape.isNotEmpty()) {
        val pts = mutableListOf<Pt>()
        for (r in curShape.indices) for (c in curShape[r].indices) if (curShape[r][c] == '#')
            pts.add(Pt(r, c))
        shapes[curId] = normalize(Piece(pts))
    }
    shapes[slackIdx] = Piece(listOf(Pt(0, 0)))
    val variations = Array(arrSize) { emptyArray<Piece>() }
    val varCnt = IntArray(arrSize)
    for (i in 0 until arrSize) {
        if (shapes[i].n > 0) {
            val vars = generateVariations(shapes[i])
            variations[i] = vars.toTypedArray()
            varCnt[i] = vars.size
        }
    }
    var solved = 0
    for (line in regionLines) {
        val parts = line.split(":")
        if (parts.size != 2) continue
        val dims = parts[0].trim()
        val countsStr = parts[1].trim()
        val dimMatch = Regex("(\\d+)x(\\d+)").matchEntire(dims) ?: continue
        val wx = dimMatch.groupValues[1].toInt()
        val h = dimMatch.groupValues[2].toInt()
        val gridSize = wx * h
        val pieceCnt = IntArray(arrSize)
        var totalArea = 0
        val toks = countsStr.split(Regex("\\s+"))
        for (idx in toks.indices) {
            val c = toks[idx].toIntOrNull() ?: 0
            if (c > 0 && idx < arrSize - 1) {
                pieceCnt[idx] = c
                totalArea += c * shapes[idx].n
            }
        }
        if (totalArea > gridSize) continue
        val slack = gridSize - totalArea
        if (slack > 0) pieceCnt[slackIdx] = slack
        val idsTmp = mutableListOf<Int>()
        for (i in 0 until arrSize) if (pieceCnt[i] > 0) idsTmp.add(i)
        idsTmp.sortByDescending { shapes[it].n }
        val ids = idsTmp.toIntArray()
        val grid = BooleanArray(gridSize)
        if (solveRec(h, wx, grid, pieceCnt, arrSize, ids, ids.size, variations, varCnt, slackIdx, shapes))
            solved++
    }
    println("Number of regions that fit all presents: $solved")
}
