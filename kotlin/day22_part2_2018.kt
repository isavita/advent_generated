import java.io.File
import java.util.*

data class Coord(val x: Int, val y: Int)

data class Item(val pos: Coord, val equip: Int, val time: Int)

const val geologicY = 16807
const val geologicX = 48271
const val caveModulo = 20183

const val TypeRocky = 0
const val TypeWet = 1
const val TypeNarrow = 2

const val ToolNone = 1 shl 0
const val ToolTorch = 1 shl 1
const val ToolGear = 1 shl 2

class Map(val target: Coord, val depth: Int) {
    val geologicIndicesCache = mutableMapOf<Int, MutableMap<Int, Int>>()
    val erosionLevelsCache = mutableMapOf<Int, MutableMap<Int, Int>>()

    fun geologicIndex(x: Int, y: Int): Int {
        if (geologicIndicesCache[y] == null) {
            geologicIndicesCache[y] = mutableMapOf()
        }
        if (geologicIndicesCache[y]!![x] == null) {
            when {
                x == 0 && y == 0 || x == target.x && y == target.y -> geologicIndicesCache[y]!![x] = 0
                y == 0 -> geologicIndicesCache[y]!![x] = x * geologicY
                x == 0 -> geologicIndicesCache[y]!![x] = y * geologicX
                else -> geologicIndicesCache[y]!![x] = erosionLevel(x - 1, y) * erosionLevel(x, y - 1)
            }
        }
        return geologicIndicesCache[y]!![x]!!
    }

    fun erosionLevel(x: Int, y: Int): Int {
        if (erosionLevelsCache[y] == null) {
            erosionLevelsCache[y] = mutableMapOf()
        }
        if (erosionLevelsCache[y]!![x] == null) {
            erosionLevelsCache[y]!![x] = (geologicIndex(x, y) + depth) % caveModulo
        }
        return erosionLevelsCache[y]!![x]!!
    }

    fun type(x: Int, y: Int): Int {
        return erosionLevel(x, y) % 3
    }

    fun neighbors(pos: Coord, equip: Int): List<Item> {
        val n = mutableListOf<Item>()
        for (c in listOf(Coord(pos.x + 1, pos.y), Coord(pos.x, pos.y + 1))) {
            if (c.x >= 0 && c.y >= 0) {
                val t = type(c.x, c.y)
                if (equip and allowed(t) != 0) {
                    n.add(Item(c, equip, 1))
                    n.add(Item(c, equip xor allowed(t), 8))
                }
            }
        }
        if (pos.x > 0) {
            val c = Coord(pos.x - 1, pos.y)
            val t = type(c.x, c.y)
            if (equip and allowed(t) != 0) {
                n.add(Item(c, equip, 1))
                n.add(Item(c, equip xor allowed(t), 8))
            }
        }
        if (pos.y > 0) {
            val c = Coord(pos.x, pos.y - 1)
            val t = type(c.x, c.y)
            if (equip and allowed(t) != 0) {
                n.add(Item(c, equip, 1))
                n.add(Item(c, equip xor allowed(t), 8))
            }
        }
        return n
    }

    fun allowed(regionType: Int): Int {
        return when (regionType) {
            TypeRocky -> ToolGear or ToolTorch
            TypeWet -> ToolGear or ToolNone
            TypeNarrow -> ToolTorch or ToolNone
            else -> throw Exception("unknown region type: $regionType")
        }
    }
}

fun rescue(input: String): Int {
    val (depth, target) = input.split("\n").map { it.split(":")[1].trim() }
    val map = Map(Coord(target.split(",")[0].toInt(), target.split(",")[1].toInt()), depth.toInt())

    val queue = PriorityQueue<Item>(compareBy { it.time })
    queue.add(Item(Coord(0, 0), ToolTorch, 0))

    val distances = mutableMapOf<Pair<Coord, Int>, Int>()
    distances[Pair(Coord(0, 0), ToolTorch)] = 0

    while (queue.isNotEmpty()) {
        val item = queue.poll()
        if (item.pos == map.target && item.equip == ToolTorch) {
            return item.time
        }
        if (item.pos.x > 8 * map.target.x || item.pos.y > 8 * map.target.y) {
            continue
        }
        if (distances[Pair(item.pos, item.equip)] != null && distances[Pair(item.pos, item.equip)]!! < item.time) {
            continue
        }
        for (n in map.neighbors(item.pos, item.equip)) {
            val d = Pair(n.pos, n.equip)
            if (distances[d] == null || item.time + n.time < distances[d]!!) {
                distances[d] = item.time + n.time
                queue.add(Item(n.pos, n.equip, item.time + n.time))
            }
        }
    }
    return 0
}

fun main() {
    val input = File("input.txt").readText()
    println(rescue(input))
}