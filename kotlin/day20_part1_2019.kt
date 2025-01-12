
import java.io.File
import java.util.*

fun main() {
    val input = File("input.txt").readLines()
    val maze = input.map { it.toCharArray() }
    val start = findPortal(maze, "AA")
    val end = findPortal(maze, "ZZ")
    val portals = findPortals(maze)
    val steps = bfs(maze, start, end, portals)
    println(steps)
}

fun findPortal(maze: List<CharArray>, label: String): Pair<Int, Int> {
    for (r in maze.indices) {
        for (c in maze[r].indices) {
            if (maze[r][c].isLetter()) {
                if (c + 1 < maze[r].size && maze[r][c + 1].isLetter()) {
                    val portalLabel = "${maze[r][c]}${maze[r][c + 1]}"
                    if (portalLabel == label) {
                        if (c + 2 < maze[r].size && maze[r][c + 2] == '.') return r to c + 2
                        if (c - 1 >= 0 && maze[r][c - 1] == '.') return r to c - 1
                    }
                }
                if (r + 1 < maze.size && maze[r + 1][c].isLetter()) {
                    val portalLabel = "${maze[r][c]}${maze[r + 1][c]}"
                    if (portalLabel == label) {
                        if (r + 2 < maze.size && maze[r + 2][c] == '.') return r + 2 to c
                        if (r - 1 >= 0 && maze[r - 1][c] == '.') return r - 1 to c
                    }
                }
            }
        }
    }
    throw IllegalArgumentException("Portal $label not found")
}

fun findPortals(maze: List<CharArray>): Map<Pair<Int, Int>, Pair<Int, Int>> {
    val portalMap = mutableMapOf<Pair<Int, Int>, Pair<Int, Int>>()
    val portalLocations = mutableMapOf<String, MutableList<Pair<Int, Int>>>()

    for (r in maze.indices) {
        for (c in maze[r].indices) {
            if (maze[r][c].isLetter()) {
                if (c + 1 < maze[r].size && maze[r][c + 1].isLetter()) {
                    val portalLabel = "${maze[r][c]}${maze[r][c + 1]}"
                    val portalPos = if (c + 2 < maze[r].size && maze[r][c + 2] == '.') r to c + 2 else r to c - 1
                    portalLocations.getOrPut(portalLabel) { mutableListOf() }.add(portalPos)
                }
                if (r + 1 < maze.size && maze[r + 1][c].isLetter()) {
                    val portalLabel = "${maze[r][c]}${maze[r + 1][c]}"
                    val portalPos = if (r + 2 < maze.size && maze[r + 2][c] == '.') r + 2 to c else r - 1 to c
                    portalLocations.getOrPut(portalLabel) { mutableListOf() }.add(portalPos)
                }
            }
        }
    }

    for ((_, locations) in portalLocations) {
        if (locations.size == 2) {
            portalMap[locations[0]] = locations[1]
            portalMap[locations[1]] = locations[0]
        }
    }
    return portalMap
}

fun bfs(maze: List<CharArray>, start: Pair<Int, Int>, end: Pair<Int, Int>, portals: Map<Pair<Int, Int>, Pair<Int, Int>>): Int {
    val queue = LinkedList<Pair<Pair<Int, Int>, Int>>()
    val visited = mutableSetOf<Pair<Int, Int>>()
    queue.add(start to 0)
    visited.add(start)

    val directions = listOf(0 to 1, 0 to -1, 1 to 0, -1 to 0)

    while (queue.isNotEmpty()) {
        val (current, steps) = queue.poll()
        if (current == end) return steps

        val (r, c) = current
        for ((dr, dc) in directions) {
            val nr = r + dr
            val nc = c + dc
            if (nr in maze.indices && nc in maze[0].indices && maze[nr][nc] == '.' && (nr to nc) !in visited) {
                queue.add(nr to nc to steps + 1)
                visited.add(nr to nc)
            }
        }
        if (current in portals) {
            val portalTarget = portals[current]!!
            if (portalTarget !in visited) {
                queue.add(portalTarget to steps + 1)
                visited.add(portalTarget)
            }
        }
    }
    return -1
}
