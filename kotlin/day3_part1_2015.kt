import java.io.File

fun main() {
    val directions = File("input.txt").readText()

    val visitedHouses = mutableSetOf<Pair<Int, Int>>()
    var x = 0
    var y = 0

    visitedHouses.add(Pair(x, y))

    directions.forEach {
        when (it) {
            '^' -> y++
            'v' -> y--
            '>' -> x++
            '<' -> x--
        }
        visitedHouses.add(Pair(x, y))
    }

    println(visitedHouses.size)
}