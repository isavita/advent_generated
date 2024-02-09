import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val triangles = input.map { it.trim().split("\\s+".toRegex()).map { it.toInt() } }

    var possibleTriangles = 0
    for (i in triangles.indices step 3) {
        for (j in 0 until 3) {
            val sides = listOf(triangles[i][j], triangles[i + 1][j], triangles[i + 2][j])
            if (sides[0] + sides[1] > sides[2] && sides[1] + sides[2] > sides[0] && sides[0] + sides[2] > sides[1]) {
                possibleTriangles++
            }
        }
    }

    println(possibleTriangles)
}