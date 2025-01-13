
import java.io.File
import java.util.*

data class Point(val x: Int, val y: Int)

data class Unit(val id: Int, val type: Char, var pos: Point, var hp: Int = 200, val attackPower: Int = 3)

fun main() {
    val lines = File("input.txt").readLines()
    val grid = lines.map { it.toCharArray() }.toTypedArray()

    val units = mutableListOf<Unit>()
    var idCounter = 0
    for (y in grid.indices) {
        for (x in grid[y].indices) {
            if (grid[y][x] == 'G' || grid[y][x] == 'E') {
                units.add(Unit(idCounter++, grid[y][x], Point(x, y)))
            }
        }
    }

    var round = 0
    while (true) {
        units.sortBy { it.pos.y * grid[0].size + it.pos.x }
        val unitsThisRound = units.toMutableList()
        for (unit in unitsThisRound) {
            if (unit.hp <= 0) continue

            if (units.none { it.hp > 0 && it.type != unit.type }) {
                val remainingHp = units.filter { it.hp > 0 }.sumOf { it.hp }
                println(round * remainingHp)
                return
            }

            val targets = units.filter { it.hp > 0 && it.type != unit.type }
            if (targets.isEmpty()) continue

            var inRange = targets.flatMap { adjacent(it.pos) }
                .filter { grid[it.y][it.x] == '.' }
                .toSet()

            if (adjacent(unit.pos).any { p -> targets.any { it.pos == p } }) {
                inRange = emptySet()
            }

            if (inRange.isNotEmpty()) {
                val reachable = mutableSetOf<Point>()
                val q: Queue<Pair<Point, Int>> = LinkedList()
                val visited = mutableSetOf<Point>()
                q.add(unit.pos to 0)
                visited.add(unit.pos)

                var minDist = Int.MAX_VALUE
                while (q.isNotEmpty()) {
                    val (curr, dist) = q.poll()
                    if (dist > minDist) break

                    if (curr in inRange) {
                        reachable.add(curr)
                        minDist = dist
                    }

                    for (adj in adjacent(curr)) {
                        if (adj !in visited && grid[adj.y][adj.x] == '.') {
                            visited.add(adj)
                            q.add(adj to dist + 1)
                        }
                    }
                }

                if (reachable.isNotEmpty()) {
                    val chosen = reachable.minWithOrNull(compareBy({ it.y }, { it.x }))!!
                    val path = findPath(unit.pos, chosen, grid)
                    if (path != null && path.size > 1) {
                        grid[unit.pos.y][unit.pos.x] = '.'
                        unit.pos = path[1]
                        grid[unit.pos.y][unit.pos.x] = unit.type
                    }
                }
            }

            val attackTargets = adjacent(unit.pos)
                .mapNotNull { p -> units.find { it.pos == p && it.type != unit.type && it.hp > 0 } }
                .filter { it.hp > 0 }

            if (attackTargets.isNotEmpty()) {
                val target = attackTargets.minWithOrNull(compareBy({ it.hp }, { it.pos.y }, { it.pos.x }))!!
                target.hp -= unit.attackPower
                if (target.hp <= 0) {
                    grid[target.pos.y][target.pos.x] = '.'
                    units.remove(target)
                }
            }
        }
        units.removeAll { it.hp <= 0 }
        round++
    }
}

fun adjacent(p: Point): List<Point> = listOf(
    Point(p.x, p.y - 1),
    Point(p.x - 1, p.y),
    Point(p.x + 1, p.y),
    Point(p.x, p.y + 1)
)

fun findPath(start: Point, end: Point, grid: Array<CharArray>): List<Point>? {
    val q: Queue<Pair<Point, List<Point>>> = LinkedList()
    val visited = mutableSetOf<Point>()
    q.add(start to listOf(start))
    visited.add(start)

    while (q.isNotEmpty()) {
        val (curr, path) = q.poll()
        if (curr == end) return path

        for (adj in adjacent(curr).sortedWith(compareBy({ it.y }, { it.x }))) {
            if (adj !in visited && grid[adj.y][adj.x] == '.') {
                visited.add(adj)
                q.add(adj to path + adj)
            }
        }
    }
    return null
}
