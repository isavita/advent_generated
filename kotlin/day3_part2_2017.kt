import java.io.File

fun main() {
    val input = File("input.txt").readText().trim().toInt()
    var x = 0
    var y = 0
    var dx = 0
    var dy = -1
    var grid = mutableMapOf<Pair<Int, Int>, Int>()
    var sum = 0

    for (i in 1..input) {
        sum = 0
        for (dx in -1..1) {
            for (dy in -1..1) {
                if (dx == 0 && dy == 0) continue
                sum += grid.getOrDefault(Pair(x + dx, y + dy), 0)
            }
        }
        if (sum == 0) sum = 1
        grid[Pair(x, y)] = sum

        if (sum > input) {
            println(sum)
            break
        }

        if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
            val temp = dx
            dx = -dy
            dy = temp
        }
        x += dx
        y += dy
    }
}