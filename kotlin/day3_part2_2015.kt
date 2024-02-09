import java.io.File

fun main() {
    val input = File("input.txt").readText()

    val houses = mutableSetOf<Pair<Int, Int>>()
    var santaX = 0
    var santaY = 0
    var roboX = 0
    var roboY = 0
    houses.add(Pair(santaX, santaY))

    var santaTurn = true
    for (i in input.indices) {
        val direction = if (santaTurn) {
            when (input[i]) {
                '^' -> Pair(0, 1)
                'v' -> Pair(0, -1)
                '>' -> Pair(1, 0)
                '<' -> Pair(-1, 0)
                else -> Pair(0, 0)
            }
        } else {
            when (input[i]) {
                '^' -> Pair(0, 1)
                'v' -> Pair(0, -1)
                '>' -> Pair(1, 0)
                '<' -> Pair(-1, 0)
                else -> Pair(0, 0)
            }
        }

        if (santaTurn) {
            santaX += direction.first
            santaY += direction.second
            houses.add(Pair(santaX, santaY))
        } else {
            roboX += direction.first
            roboY += direction.second
            houses.add(Pair(roboX, roboY))
        }

        santaTurn = !santaTurn
    }

    println(houses.size)
}