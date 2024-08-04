import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val visited = mutableSetOf<Pair<Int, Int>>()
    val knots = Array(10) { Pair(0, 0) }

    visited.add(knots.last())

    input.forEach { line ->
        val (direction, steps) = line.split(" ")
        repeat(steps.toInt()) {
            when (direction) {
                "R" -> knots[0] = knots[0].copy(first = knots[0].first + 1)
                "L" -> knots[0] = knots[0].copy(first = knots[0].first - 1)
                "U" -> knots[0] = knots[0].copy(second = knots[0].second + 1)
                "D" -> knots[0] = knots[0].copy(second = knots[0].second - 1)
            }

            for (i in 1 until knots.size) {
                val head = knots[i - 1]
                val tail = knots[i]

                if (Math.abs(head.first - tail.first) > 1 || Math.abs(head.second - tail.second) > 1) {
                    if (head.first != tail.first) {
                        knots[i] = knots[i].copy(first = if (head.first > tail.first) tail.first + 1 else tail.first - 1)
                    }
                    if (head.second != tail.second) {
                        knots[i] = knots[i].copy(second = if (head.second > tail.second) tail.second + 1 else tail.second - 1)
                    }
                }
            }

            visited.add(knots.last())
        }
    }

    println(visited.size)
}