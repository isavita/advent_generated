
import java.io.File
import java.util.*
import kotlin.Comparator
import kotlin.collections.HashMap

data class Move(val label: String, val x: Int, val y: Int)

fun main() {
    val input = File("input.txt").readLines()
    var graph = input.filter { it.isNotEmpty() }.map { it.toCharArray().toList() }

    val H = graph.size
    val W = graph.firstOrNull()?.size ?: 0

    val move = listOf(
        Move("left", -1, 0),
        Move("up", 0, -1),
        Move("right", 1, 0),
        Move("down", 0, 1)
    )

    var sum = 0

    for (y in 0 until H) {
        for (x in 0 until W) {
            if (graph[y][x] == '.') {
                continue
            }

            var area = 0
            val target = graph[y][x]
            val visited = mutableSetOf<Pair<Int, Int>>()
            val side = mutableMapOf<String, MutableSet<Pair<Int, Int>>>()

            fun search(cx: Int, cy: Int, label: String) {
                if (graph[cy][cx] != target) {
                    if (label.isNotEmpty() && !visited.contains(Pair(cx, cy))) {
                        saveOuter(label, side, cx, cy)
                    }
                    return
                }

                visited.add(Pair(cx, cy))
                area++
                graph = graph.mapIndexed { rowIdx, row ->
                    if (rowIdx == cy) {
                        row.mapIndexed { colIdx, char -> if (colIdx == cx) '.' else char }
                    } else {
                        row
                    }
                }

                for (m in move) {
                    val nx = cx + m.x
                    val ny = cy + m.y

                    if (nx < 0 || nx >= W || ny < 0 || ny >= H) {
                        saveOuter(m.label, side, nx, ny)
                        continue
                    }
                    search(nx, ny, m.label)
                }
            }

            search(x, y, "")
            val outer = countOuter(side)
            sum += area * outer
        }
    }
    println(sum)
}

fun saveOuter(label: String, side: MutableMap<String, MutableSet<Pair<Int, Int>>>, x: Int, y: Int) {
    val key = if (label == "up" || label == "down") Pair(y, x) else Pair(x, y)
    side.getOrPut(label) { mutableSetOf() }.add(key)
}

fun countOuter(side: MutableMap<String, MutableSet<Pair<Int, Int>>>): Int {
    var outer = 0
    for (label in side.keys) {
        val array = side[label]!!.toMutableList()
        array.sortWith(Comparator { a, b ->
            if (a.first == b.first) {
                a.second.compareTo(b.second)
            } else {
                a.first.compareTo(b.first)
            }
        })

        val temp = mutableListOf<Pair<Int, Int>>()
        for (current in array) {
            if (!check(temp, current.first, current.second)) {
                outer++
            }
            temp.add(current)
        }
    }
    return outer
}

fun check(ary: List<Pair<Int, Int>>, i: Int, j: Int): Boolean {
    val search = listOf(Pair(i, j - 1), Pair(i, j + 1))
    for (s in search) {
        if (ary.contains(s)) {
            return true
        }
    }
    return false
}
