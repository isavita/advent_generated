import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    val grid = mutableMapOf<Pair<Int, Int>, Char>()
    val parts = mutableListOf<Part>()
    var curr: Part? = null

    input.lines().forEachIndexed { y, line ->
        if (curr != null) {
            parts.add(curr!!)
            curr = null
        }
        line.forEachIndexed { x, c ->
            grid[Pair(x, y)] = c
            if (c in '0'..'9') {
                if (curr == null) {
                    curr = Part(y, x, x, c - '0')
                } else {
                    curr!!.n *= 10
                    curr!!.n += c - '0'
                    curr!!.xmax = x
                }
            } else if (curr != null) {
                parts.add(curr!!)
                curr = null
            }
        }
    }

    val partsGrid = mutableMapOf<Pair<Int, Int>, Int>()
    parts.forEachIndexed { i, p ->
        (p.xmin..p.xmax).forEach { x ->
            partsGrid[Pair(x, p.y)] = i
        }
    }

    var sum = 0
    grid.forEach { p, c ->
        if (c == '*') {
            val neighborParts = mutableSetOf<Int>()
            listOf(Pair(0, 1), Pair(0, -1), Pair(1, 0), Pair(-1, 0),
                Pair(-1, -1), Pair(-1, 1), Pair(1, -1), Pair(1, 1)).forEach { n ->
                partsGrid[Pair(n.first + p.first, n.second + p.second)]?.let { i ->
                    neighborParts.add(i)
                }
            }
            if (neighborParts.size == 2) {
                var prod = 1
                neighborParts.forEach { i ->
                    prod *= parts[i].n
                }
                sum += prod
            }
        }
    }
    println(sum)
}

data class Part(val y: Int, var xmin: Int, var xmax: Int, var n: Int)