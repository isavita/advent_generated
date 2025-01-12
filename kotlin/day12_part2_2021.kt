
import java.io.File

fun main() {
    val connections = File("input.txt").readLines().flatMap { line ->
        val (a, b) = line.split("-")
        listOf(a to b, b to a)
    }.groupBy({ it.first }, { it.second })

    fun isSmallCave(cave: String) = cave[0].isLowerCase()

    fun solve(allowDoubleVisit: Boolean): Int {
        var pathCount = 0

        fun traverse(current: String, visited: MutableMap<String, Int>, doubleVisited: Boolean) {
            if (current == "end") {
                pathCount++
                return
            }

            connections[current]?.forEach { next ->
                if (next == "start") return@forEach

                val nextVisitedCount = visited.getOrDefault(next, 0)
                if (isSmallCave(next)) {
                    if (nextVisitedCount == 0) {
                        visited[next] = 1
                        traverse(next, visited, doubleVisited)
                        visited[next] = 0
                    } else if (allowDoubleVisit && !doubleVisited && nextVisitedCount == 1) {
                        visited[next] = 2
                        traverse(next, visited, true)
                        visited[next] = 1
                    }
                } else {
                    traverse(next, visited, doubleVisited)
                }
            }
        }

        traverse("start", mutableMapOf("start" to 1), false)
        return pathCount
    }

    println("Part 1: ${solve(false)}")
    println("Part 2: ${solve(true)}")
}
