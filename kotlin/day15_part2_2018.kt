
import java.io.File
import java.util.*

data class Point(val x: Int, val y: Int)

data class Unit(val id: Int, val type: Char, var pos: Point, var hp: Int = 200, var ap: Int = 3)

fun main() {
    val lines = File("input.txt").readLines()

    println("Part 1: ${solve(lines, 3, false)}")
    println("Part 2: ${solvePart2(lines)}")
}

fun solve(input: List<String>, elfAp: Int, noElfDeaths: Boolean): Int {
    var grid = input.map { it.toCharArray() }.toMutableList()
    val units = mutableListOf<Unit>()
    var idCounter = 0

    for ((y, row) in grid.withIndex()) {
        for ((x, cell) in row.withIndex()) {
            if (cell == 'G' || cell == 'E') {
                units.add(Unit(idCounter++, cell, Point(x, y), ap = if (cell == 'E') elfAp else 3))
            }
        }
    }
    val initialElfCount = units.count { it.type == 'E' }

    var rounds = 0
    while (true) {
        units.sortBy { it.pos.y * grid[0].size + it.pos.x }
        var combatEnded = false
        for (unit in units.filter { it.hp > 0 }) {
            if (units.none { it.hp > 0 && it.type != unit.type }) {
                combatEnded = true
                break
            }

            if (unit.hp <= 0) continue

            val targets = units.filter { it.hp > 0 && it.type != unit.type }
            if (targets.isEmpty()) {
                combatEnded = true
                break
            }

            var inRange = targets.filter { isAdjacent(unit.pos, it.pos) }
            if (inRange.isEmpty()) {
                val targetPos = findTarget(grid, unit, targets)
                if (targetPos != null) {
                    val nextPos = moveTowards(grid, unit.pos, targetPos)
                    if (nextPos != null) {
                        grid[unit.pos.y][unit.pos.x] = '.'
                        unit.pos = nextPos
                        grid[unit.pos.y][unit.pos.x] = unit.type
                    }
                }
                inRange = targets.filter { isAdjacent(unit.pos, it.pos) }
            }

            if (inRange.isNotEmpty()) {
                val target = inRange.minWithOrNull(compareBy<Unit> { it.hp }.thenBy { it.pos.y }.thenBy { it.pos.x })!!
                target.hp -= unit.ap
                if (target.hp <= 0) {
                    grid[target.pos.y][target.pos.x] = '.'
                }
            }
        }

        if (combatEnded) {
            if (noElfDeaths && units.count { it.type == 'E' && it.hp > 0 } < initialElfCount) {
                return -1
            }
            val remainingHp = units.filter { it.hp > 0 }.sumOf { it.hp }
            return rounds * remainingHp
        }
        rounds++
    }
}

fun solvePart2(input: List<String>): Int {
    var elfAp = 4
    while (true) {
        val result = solve(input, elfAp, true)
        if (result != -1) {
            return result
        }
        elfAp++
    }
}

fun isAdjacent(p1: Point, p2: Point): Boolean {
    return (p1.x == p2.x && Math.abs(p1.y - p2.y) == 1) || (p1.y == p2.y && Math.abs(p1.x - p2.x) == 1)
}

fun findTarget(grid: List<CharArray>, unit: Unit, targets: List<Unit>): Point? {
    val inRange = targets.flatMap { target ->
        listOf(
            Point(target.pos.x, target.pos.y - 1),
            Point(target.pos.x - 1, target.pos.y),
            Point(target.pos.x + 1, target.pos.y),
            Point(target.pos.x, target.pos.y + 1)
        ).filter {
            it.y in grid.indices && it.x in grid[0].indices && grid[it.y][it.x] == '.'
        }
    }.toSet()

    if (inRange.isEmpty()) return null

    val reachable = mutableMapOf<Point, Int>()
    val q: Queue<Pair<Point, Int>> = LinkedList()
    q.add(Pair(unit.pos, 0))
    val visited = mutableSetOf<Point>()
    visited.add(unit.pos)

    while (q.isNotEmpty()) {
        val (curr, dist) = q.poll()
        if (curr in inRange) {
            reachable[curr] = dist
        }

        listOf(
            Point(curr.x, curr.y - 1),
            Point(curr.x - 1, curr.y),
            Point(curr.x + 1, curr.y),
            Point(curr.x, curr.y + 1)
        ).filter {
            it.y in grid.indices && it.x in grid[0].indices && grid[it.y][it.x] == '.' && it !in visited
        }.forEach {
            q.add(Pair(it, dist + 1))
            visited.add(it)
        }
    }

    if (reachable.isEmpty()) return null

    val minDist = reachable.values.minOrNull()!!
    return reachable.filter { it.value == minDist }.keys.minWithOrNull(compareBy { it.y * grid[0].size + it.x })
}

fun moveTowards(grid: List<CharArray>, start: Point, end: Point): Point? {
    val q: Queue<Pair<Point, List<Point>>> = LinkedList()
    q.add(Pair(start, listOf(start)))
    val visited = mutableSetOf<Point>()
    visited.add(start)
    val paths = mutableListOf<List<Point>>()

    while (q.isNotEmpty()) {
        val (curr, path) = q.poll()
        if (curr == end) {
            paths.add(path)
        }

        listOf(
            Point(curr.x, curr.y - 1),
            Point(curr.x - 1, curr.y),
            Point(curr.x + 1, curr.y),
            Point(curr.x, curr.y + 1)
        ).filter {
            it.y in grid.indices && it.x in grid[0].indices && (grid[it.y][it.x] == '.' || it == end) && it !in visited
        }.forEach {
            q.add(Pair(it, path + it))
            visited.add(it)
        }
    }

    if (paths.isEmpty()) return null

    val shortestPath = paths.minByOrNull { it.size }!!
    return shortestPath.getOrNull(1)
}
