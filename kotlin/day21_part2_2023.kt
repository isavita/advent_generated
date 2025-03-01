
import java.io.File
import kotlin.math.absoluteValue

fun main() {
    val lines = File("input.txt").readLines()
    val garden = mutableSetOf<Pair<Int, Int>>()
    var start = Pair(0, 0)
    lines.forEachIndexed { y, line ->
        line.forEachIndexed { x, c ->
            if (c != '#') {
                garden.add(Pair(x, y))
                if (c == 'S') {
                    start = Pair(x, y)
                }
            }
        }
    }

    val maxSize = lines.size
    val numIterations = 26501365

    fun complexMod(pos: Pair<Int, Int>): Pair<Int, Int> {
        return Pair((pos.first % maxSize + maxSize) % maxSize, (pos.second % maxSize + maxSize) % maxSize)
    }

    fun calculateNumEnds(): Long {
        val queue = mutableSetOf(start)
        val done = mutableListOf<Int>()

        for (i in 0 until 3 * maxSize) {
            if ((i % maxSize) == (maxSize - 1) / 2) {
                done.add(queue.size)
            }
            if (done.size == 3) {
                break
            }

            val newQueue = mutableSetOf<Pair<Int, Int>>()
            for (point in queue) {
                for (dir in listOf(Pair(1, 0), Pair(-1, 0), Pair(0, 1), Pair(0, -1))) {
                    val next = Pair(point.first + dir.first, point.second + dir.second)
                    if (complexMod(next) in garden) {
                        newQueue.add(next)
                    }
                }
            }
            queue.clear()
            queue.addAll(newQueue)
        }

        fun quadraticFunction(n: Long, a: Int, b: Int, c: Int): Long {
            return a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2))
        }

        return quadraticFunction((numIterations / maxSize).toLong(), done[0], done[1], done[2])
    }

    println(calculateNumEnds())
}
