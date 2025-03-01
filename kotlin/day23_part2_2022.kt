
import java.io.File

data class Elf(var pos: Pair<Int, Int>, var moving: Boolean = false, var nextPos: Pair<Int, Int>? = null)

fun Elf.aroundAllEmpty(map: MutableSet<Pair<Int, Int>>, dirs: List<Pair<Int, Int>>): Boolean {
    return dirs.all { (dx, dy) -> (pos.first + dx to pos.second + dy) !in map }
}

fun Elf.elfInDirection(wannaGo: Int, map: MutableSet<Pair<Int, Int>>, dirs: List<Pair<Int, Int>>): Boolean {
    for (j in -1..1) {
        val dxy = dirs[(wannaGo + j + 8) % 8]
        if ((pos.first + dxy.first to pos.second + dxy.second) in map) return true
    }
    return false
}

fun parse(): Pair<MutableList<Elf>, MutableSet<Pair<Int, Int>>> {
    val elves = mutableListOf<Elf>()
    val map = mutableSetOf<Pair<Int, Int>>()
    File("input.txt").readLines().forEachIndexed { row, line ->
        line.forEachIndexed { col, char ->
            if (char == '#') {
                val pos = row to col
                map.add(pos)
                elves.add(Elf(pos))
            }
        }
    }
    return elves to map
}

fun run(elves: MutableList<Elf>, map: MutableSet<Pair<Int, Int>>, order: List<Int>, currDir: Int, dirs: List<Pair<Int, Int>>): Boolean {
    val proposes = mutableMapOf<Pair<Int, Int>, Int>()
    for (e in elves) {
        if (e.aroundAllEmpty(map, dirs)) continue
        for (i in 0 until 4) {
            val dir = order[(currDir + i) % 4]
            if (e.elfInDirection(dir, map, dirs)) continue
            val dxy = dirs[dir]
            val dest = e.pos.first + dxy.first to e.pos.second + dxy.second
            proposes[dest] = proposes.getOrDefault(dest, 0) + 1
            e.nextPos = dest
            e.moving = true
            break
        }
    }

    var someoneMoved = false
    for (e in elves) {
        if (!e.moving) continue
        if (proposes[e.nextPos]!! > 1) {
            e.moving = false
            continue
        }
        someoneMoved = true
        map.remove(e.pos)
        map.add(e.nextPos!!)
        e.pos = e.nextPos!!
        e.moving = false
    }
    elves.forEach { it.nextPos = null }

    return someoneMoved
}

fun main() {
    val dirs = listOf(-1 to -1, -1 to 0, -1 to 1, 0 to 1, 1 to 1, 1 to 0, 1 to -1, 0 to -1)
    val order = listOf(1, 5, 7, 3)
    var currDir = 0

    val (elves, map) = parse()

    var i = 0
    while (true) {
        if (!run(elves, map, order, currDir, dirs)) {
            println(i + 1)
            break
        }
        currDir = (currDir + 1) % 4
        i++
    }
}
