
import java.awt.Point
import java.io.File
import java.util.*
import java.util.regex.Pattern

data class Node(var used: Int, var avail: Int)

data class Item<T>(val obj: T, val priority: Int) : Comparable<Item<T>> {
    override fun compareTo(other: Item<T>): Int = other.priority - this.priority
}

val neighbors4 = listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))
val re = Pattern.compile("-x(\\d+)-y(\\d+)")

fun main() {
    val nodes = mutableMapOf<Point, Node>()
    val input = File("input.txt").readText().trim().split("\n").drop(2)

    for (line in input) {
        val parts = line.split("\\s+".toRegex())
        val matcher = re.matcher(parts[0])
        matcher.find()
        val x = matcher.group(1).toInt()
        val y = matcher.group(2).toInt()
        val p = Point(x, y)
        val used = parts[2].dropLast(1).toInt()
        val avail = parts[3].dropLast(1).toInt()
        nodes[p] = Node(used, avail)
    }
    println(minMoves(nodes))
}

fun minMoves(nodes: MutableMap<Point, Node>): Int {
    val (w, _) = dim(nodes)
    var goal = Point(w, 0)
    var hole = findHole(nodes) ?: throw Exception("No hole found")

    var sum = 0
    while (goal != Point(0, 0)) {
        val next = Point(goal.x - 1, goal.y)
        sum += moves(nodes, goal, hole, next)
        hole = next
        sum += moves(nodes, goal, goal, hole)
        val temp = hole
        hole = goal
        goal = temp
    }
    return sum
}

fun findHole(nodes: MutableMap<Point, Node>): Point? {
    return nodes.entries.firstOrNull { it.value.used == 0 }?.key
}

fun moves(nodes: MutableMap<Point, Node>, goal: Point, from: Point, to: Point): Int {
    val (w, h) = dim(nodes)
    val depth = mutableMapOf(from to 0)
    val pq = PriorityQueue<Item<Point>>()
    pq.add(Item(from, 0))

    while (pq.isNotEmpty()) {
        val current = pq.poll().obj
        if (current == to) {
            return depth[current]!!
        }
        val currentDepth = depth[current]!! + 1
        for (n in neighbors4) {
            val next = Point(current.x + n.x, current.y + n.y)
            if (next.x < 0 || next.y < 0 || next.x > w || next.y > h || nodes[next]?.used ?: Int.MAX_VALUE > 400 || next == goal) {
                continue
            }
            if (depth[next] == null || currentDepth < depth[next]!!) {
                depth[next] = currentDepth
                pq.add(Item(next, -currentDepth))
            }
        }
    }
    throw Exception("No path found")
}

fun dim(m: MutableMap<Point, Node>): Pair<Int, Int> {
    var w = 0
    var h = 0
    for (p in m.keys) {
        if (p.x > w) {
            w = p.x
        }
        if (p.y > h) {
            h = p.y
        }
    }
    return Pair(w, h)
}
