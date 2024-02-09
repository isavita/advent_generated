import java.io.File

data class Point(val x: Int, val y: Int)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    var head = Point(0, 0)
    var tail = Point(0, 0)
    val visited = mutableMapOf(tail to true)

    lines.forEach { line ->
        val parts = line.split(" ")
        val dir = parts[0]
        val steps = parts[1].toInt()

        repeat(steps) {
            when (dir) {
                "R" -> head = head.copy(x = head.x + 1)
                "L" -> head = head.copy(x = head.x - 1)
                "U" -> head = head.copy(y = head.y + 1)
                "D" -> head = head.copy(y = head.y - 1)
            }

            if (Math.abs(head.x - tail.x) > 1 || Math.abs(head.y - tail.y) > 1) {
                if (head.x != tail.x && head.y != tail.y) {
                    tail = if (head.x > tail.x) tail.copy(x = tail.x + 1) else tail.copy(x = tail.x - 1)
                    tail = if (head.y > tail.y) tail.copy(y = tail.y + 1) else tail.copy(y = tail.y - 1)
                } else {
                    tail = if (head.x > tail.x) tail.copy(x = tail.x + 1) else if (head.x < tail.x) tail.copy(x = tail.x - 1) else tail
                    tail = if (head.y > tail.y) tail.copy(y = tail.y + 1) else if (head.y < tail.y) tail.copy(y = tail.y - 1) else tail
                }
            }

            visited[tail] = true
        }
    }

    println(visited.size)
}