
import java.io.File
import java.util.StringTokenizer
import java.util.Collections

data class Point(val x: Int, val y: Int, val z: Int)

data class Edge(val u: Int, val v: Int, val d: Long)

private class DSU(n: Int) {
    private val parent = IntArray(n) { it }
    private val rank = IntArray(n)

    fun find(x: Int): Int {
        var v = x
        while (parent[v] != v) {
            parent[v] = parent[parent[v]]
            v = parent[v]
        }
        return v
    }

    fun union(a: Int, b: Int): Boolean {
        var ra = find(a)
        var rb = find(b)
        if (ra == rb) return false
        if (rank[ra] < rank[rb]) {
            parent[ra] = rb
        } else if (rank[ra] > rank[rb]) {
            parent[rb] = ra
        } else {
            parent[rb] = ra
            rank[ra]++
        }
        return true
    }
}

fun distSq(p1: Point, p2: Point): Long {
    val dx = (p1.x - p2.x).toLong()
    val dy = (p1.y - p2.y).toLong()
    val dz = (p1.z - p2.z).toLong()
    return dx * dx + dy * dy + dz * dz
}

fun main() {
    val file = File("input.txt")
    if (!file.exists()) return

    val points = mutableListOf<Point>()
    file.forEachLine { line ->
        val trimmed = line.trim()
        if (trimmed.isEmpty()) return@forEachLine
        val st = StringTokenizer(trimmed, ",")
        if (st.countTokens() != 3) return@forEachLine
        val x = st.nextToken().toInt()
        val y = st.nextToken().toInt()
        val z = st.nextToken().toInt()
        points.add(Point(x, y, z))
    }

    val n = points.size
    if (n < 2) return

    val edges = ArrayList<Edge>(n * (n - 1) / 2)
    for (i in 0 until n) {
        for (j in i + 1 until n) {
            edges.add(Edge(i, j, distSq(points[i], points[j])))
        }
    }
    edges.sortBy { it.d }

    val dsu = DSU(n)
    var components = n
    for (e in edges) {
        if (dsu.union(e.u, e.v)) {
            if (--components == 1) {
                val p1 = points[e.u]
                val p2 = points[e.v]
                println("Connected ${p1.x},${p1.y},${p1.z} and ${p2.x},${p2.y},${p2.z}")
                println("Product of X coordinates: ${p1.x.toLong() * p2.x}")
                break
            }
        }
    }
}
