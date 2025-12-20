import java.io.File
import java.util.ArrayDeque
import kotlin.math.abs

data class Point(val x: Int, val y: Int)

fun main() {
    val lines = File("input.txt").readLines()
    val pts = mutableListOf<Point>()
    val xs = mutableListOf<Int>()
    val ys = mutableListOf<Int>()
    for (line in lines) {
        val parts = line.split(',').map { it.trim() }
        if (parts.size != 2) continue
        val x = parts[0].toIntOrNull() ?: continue
        val y = parts[1].toIntOrNull() ?: continue
        pts.add(Point(x, y))
        xs.add(x)
        ys.add(y)
    }
    if (pts.isEmpty()) {
        println("Largest valid area: 0")
        return
    }

    val ux = xs.distinct().sorted()
    val uy = ys.distinct().sorted()
    val xIdx = HashMap<Int, Int>(ux.size)
    val yIdx = HashMap<Int, Int>(uy.size)
    for (i in ux.indices) xIdx[ux[i]] = i
    for (i in uy.indices) yIdx[uy[i]] = i

    val W = 2 * ux.size + 1
    val H = 2 * uy.size + 1
    val colW = LongArray(W)
    val rowH = LongArray(H)
    colW[0] = 1
    for (i in ux.indices) {
        colW[2 * i + 1] = 1
        if (i + 1 < ux.size) {
            var gap = ux[i + 1] - ux[i] - 1
            if (gap < 0) gap = 0
            colW[2 * i + 2] = gap.toLong()
        } else {
            colW[2 * i + 2] = 1
        }
    }
    rowH[0] = 1
    for (i in uy.indices) {
        rowH[2 * i + 1] = 1
        if (i + 1 < uy.size) {
            var gap = uy[i + 1] - uy[i] - 1
            if (gap < 0) gap = 0
            rowH[2 * i + 2] = gap.toLong()
        } else {
            rowH[2 * i + 2] = 1
        }
    }

    val grid = Array(H) { ByteArray(W) }

    for (i in pts.indices) {
        val a = pts[i]
        val b = pts[(i + 1) % pts.size]
        val gx1 = 2 * xIdx[a.x]!! + 1
        val gy1 = 2 * yIdx[a.y]!! + 1
        val gx2 = 2 * xIdx[b.x]!! + 1
        val gy2 = 2 * yIdx[b.y]!! + 1
        if (gx1 == gx2) {
            val y0 = if (gy1 < gy2) gy1 else gy2
            val y1 = if (gy1 > gy2) gy1 else gy2
            for (y in y0..y1) if (rowH[y] > 0) grid[y][gx1] = 1
        } else {
            val x0 = if (gx1 < gx2) gx1 else gx2
            val x1 = if (gx1 > gx2) gx1 else gx2
            for (x in x0..x1) if (colW[x] > 0) grid[gy1][x] = 1
        }
    }

    val qx = IntArray(W * H)
    val qy = IntArray(W * H)
    var head = 0
    var tail = 0
    qx[tail] = 0
    qy[tail] = 0
    tail++
    grid[0][0] = 2
    val dirs = arrayOf(intArrayOf(0, 1), intArrayOf(0, -1), intArrayOf(1, 0), intArrayOf(-1, 0))
    while (head < tail) {
        val cx = qx[head]
        val cy = qy[head]
        head++
        for (d in dirs) {
            val nx = cx + d[0]
            val ny = cy + d[1]
            if (nx in 0 until W && ny in 0 until H && grid[ny][nx] == 0.toByte()) {
                grid[ny][nx] = 2
                qx[tail] = nx
                qy[tail] = ny
                tail++
            }
        }
    }

    val P = Array(H) { LongArray(W) }
    for (y in 0 until H) {
        for (x in 0 until W) {
            val v = if (grid[y][x].toInt() != 2) colW[x] * rowH[y] else 0L
            val left = if (x > 0) P[y][x - 1] else 0L
            val up = if (y > 0) P[y - 1][x] else 0L
            val diag = if (x > 0 && y > 0) P[y - 1][x - 1] else 0L
            P[y][x] = v + left + up - diag
        }
    }

    var maxArea = 0L
    for (i in pts.indices) {
        for (j in i until pts.size) {
            val a = pts[i]
            val b = pts[j]
            val w = abs(a.x - b.x).toLong() + 1
            val h = abs(a.y - b.y).toLong() + 1
            val area = w * h
            if (area <= maxArea) continue
            var gx1 = 2 * xIdx[a.x]!! + 1
            var gy1 = 2 * yIdx[a.y]!! + 1
            var gx2 = 2 * xIdx[b.x]!! + 1
            var gy2 = 2 * yIdx[b.y]!! + 1
            if (gx1 > gx2) { val t = gx1; gx1 = gx2; gx2 = t }
            if (gy1 > gy2) { val t = gy1; gy1 = gy2; gy2 = t }
            val total = P[gy2][gx2]
            val left = if (gx1 > 0) P[gy2][gx1 - 1] else 0L
            val up = if (gy1 > 0) P[gy1 - 1][gx2] else 0L
            val diag = if (gx1 > 0 && gy1 > 0) P[gy1 - 1][gx1 - 1] else 0L
            val valid = total - left - up + diag
            if (valid == area) maxArea = area
        }
    }

    println("Largest valid area: $maxArea")
}