import java.io.File
import java.util.StringTokenizer

data class Point(val x: Int, val y: Int, val z: Int)
data class Edge(val u: Int, val v: Int, val d: Long)

fun find(parent: IntArray, x: Int): Int {
    var v = x
    while (parent[v] != v) {
        parent[v] = parent[parent[v]]
        v = parent[v]
    }
    return v
}

fun union(parent: IntArray, size: IntArray, a: Int, b: Int) {
    var ra = find(parent, a)
    var rb = find(parent, b)
    if (ra == rb) return
    if (size[ra] < size[rb]) {
        val t = ra; ra = rb; rb = t
    }
    parent[rb] = ra
    size[ra] += size[rb]
}

fun main() {
    val pts = mutableListOf<Point>()
    File("input.txt").forEachLine { line ->
        val st = StringTokenizer(line, ",")
        if (st.countTokens() == 3) {
            val x = st.nextToken().trim().toInt()
            val y = st.nextToken().trim().toInt()
            val z = st.nextToken().trim().toInt()
            pts.add(Point(x, y, z))
        }
    }
    val n = pts.size
    if (n < 2) return
    val edges = ArrayList<Edge>(n * (n - 1) / 2)
    for (i in 0 until n) {
        val pi = pts[i]
        for (j in i + 1 until n) {
            val pj = pts[j]
            val dx = (pi.x - pj.x).toLong()
            val dy = (pi.y - pj.y).toLong()
            val dz = (pi.z - pj.z).toLong()
            edges.add(Edge(i, j, dx * dx + dy * dy + dz * dz))
        }
    }
    edges.sortBy { it.d }
    val parent = IntArray(n) { it }
    val sz = IntArray(n) { 1 }
    val limit = if (edges.size < 1000) edges.size else 1000
    for (i in 0 until limit) {
        val e = edges[i]
        union(parent, sz, e.u, e.v)
    }
    val top = IntArray(3)
    for (i in 0 until n) {
        if (parent[i] == i) {
            val s = sz[i]
            when {
                s > top[0] -> { top[2] = top[1]; top[1] = top[0]; top[0] = s }
                s > top[1] -> { top[2] = top[1]; top[1] = s }
                s > top[2] -> { top[2] = s }
            }
        }
    }
    var result = 1UL
    for (i in 0..2) if (top[i] > 0) result *= top[i].toULong()
    println("Product of three largest circuit sizes: $result")
}