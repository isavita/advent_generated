import java.io.File

data class Point(val x: Int, val y: Int, val z: Int, val t: Int)

fun abs(x: Int): Int = if (x < 0) -x else x

fun manhattanDistance(a: Point, b: Point): Int = abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t)

class UnionFind(size: Int) {
    val parent = IntArray(size) { it }

    fun find(x: Int): Int {
        if (parent[x] != x) {
            parent[x] = find(parent[x])
        }
        return parent[x]
    }

    fun union(x: Int, y: Int) {
        val rootX = find(x)
        val rootY = find(y)
        if (rootX != rootY) {
            parent[rootX] = rootY
        }
    }
}

fun main(args: Array<String>) {
    val points = mutableListOf<Point>()
    File("input.txt").forEachLine {
        val coords = it.split(",")
        val x = coords[0].toInt()
        val y = coords[1].toInt()
        val z = coords[2].toInt()
        val t = coords[3].toInt()
        points.add(Point(x, y, z, t))
    }

    val uf = UnionFind(points.size)
    for (i in points.indices) {
        for (j in points.indices) {
            if (manhattanDistance(points[i], points[j]) <= 3) {
                uf.union(i, j)
            }
        }
    }

    var constellationCount = 0
    for (i in uf.parent.indices) {
        if (i == uf.find(i)) {
            constellationCount++
        }
    }
    println(constellationCount)
}