
import java.io.File
import java.util.LinkedList
import java.util.Queue

data class Point(val row: Int, val col: Int)

data class State(val pos: Point, val time: Int)

fun main() {
    val lines = File("input.txt").readLines()
    val grid = lines.map { it.toCharArray() }
    val rows = grid.size
    val cols = grid[0].size

    val start = Point(0, 1)
    val end = Point(rows - 1, cols - 2)

    fun getBlizzards(time: Int): Set<Point> {
        val blizzards = mutableSetOf<Point>()
        for (r in 1 until rows - 1) {
            for (c in 1 until cols - 1) {
                when (grid[r][c]) {
                    '>' -> blizzards.add(Point(r, 1 + (c - 1 + time) % (cols - 2)))
                    '<' -> blizzards.add(Point(r, 1 + (c - 1 - time % (cols - 2) + (cols - 2) * (time / (cols-2) + 1)) % (cols - 2)))
                    'v' -> blizzards.add(Point(1 + (r - 1 + time) % (rows - 2), c))
                    '^' -> blizzards.add(Point(1 + (r - 1 - time % (rows - 2) + (rows-2) * (time/(rows-2) + 1)) % (rows - 2), c))
                }
            }
        }
        return blizzards
    }

    fun solve(): Int {
        val queue: Queue<State> = LinkedList()
        queue.offer(State(start, 0))
        val visited = mutableSetOf<State>()
        visited.add(State(start, 0))

        while (queue.isNotEmpty()) {
            val current = queue.poll()

            if (current.pos == end) {
                return current.time
            }

            val nextTime = current.time + 1
            val nextBlizzards = getBlizzards(nextTime)

            val moves = listOf(
                Point(0, 0),  // Wait
                Point(0, 1),
                Point(0, -1),
                Point(1, 0),
                Point(-1, 0)
            )

            for (move in moves) {
                val nextPos = Point(current.pos.row + move.row, current.pos.col + move.col)
                val nextState = State(nextPos, nextTime)
                
                if ((nextPos.row in 0 until rows && nextPos.col in 0 until cols) &&
                    grid[nextPos.row][nextPos.col] != '#' &&
                    nextPos !in nextBlizzards &&
                    nextState !in visited
                ) {
                    
                    queue.offer(nextState)
                    visited.add(nextState)
                }
            }
        }
        return -1 // Should not happen according to the problem statement
    }

    println(solve())
}
