import java.io.File

fun main(args: Array<String>) {
    val cubes = mutableMapOf<Pt3, Unit>()
    val neighbors = listOf(
        Pt3(-1, 0, 0),
        Pt3(1, 0, 0),
        Pt3(0, -1, 0),
        Pt3(0, 1, 0),
        Pt3(0, 0, -1),
        Pt3(0, 0, 1)
    )
    var min = Pt3(Int.MAX_VALUE, Int.MAX_VALUE, Int.MAX_VALUE)
    var max = Pt3(Int.MIN_VALUE, Int.MIN_VALUE, Int.MIN_VALUE)

    File("input.txt").forEachLine { line ->
        if (line.isNotBlank()) {
            val cube = Pt3(0, 0, 0)
            val values = line.split(",").map { it.toInt() }
            cube.X = values[0]
            cube.Y = values[1]
            cube.Z = values[2]
            cubes[cube] = Unit
            min = Pt3(minOf(min.X, cube.X), minOf(min.Y, cube.Y), minOf(min.Z, cube.Z))
            max = Pt3(maxOf(max.X, cube.X), maxOf(max.Y, cube.Y), maxOf(max.Z, cube.Z))
        }
    }
    min = min.add(Pt3(-1, -1, -1))
    max = max.add(Pt3(1, 1, 1))

    var faces = 0
    val q = mutableListOf(Pt3(min.X, min.Y, min.Z))
    val seen = mutableMapOf(Pt3(min.X, min.Y, min.Z) to Unit)
    while (q.isNotEmpty()) {
        val curr = q.removeAt(0)
        for (delta in neighbors) {
            val next = curr.add(delta)
            if (next.X < min.X ||
                next.Y < min.Y ||
                next.Z < min.Z ||
                next.X > max.X ||
                next.Y > max.Y ||
                next.Z > max.Z
            ) {
                continue
            }
            if (cubes.containsKey(next)) {
                faces++
            } else if (!seen.containsKey(next)) {
                seen[next] = Unit
                q.add(next)
            }
        }
    }
    println(faces)
}

data class Pt3(var X: Int, var Y: Int, var Z: Int) {
    fun add(other: Pt3): Pt3 {
        return Pt3(X + other.X, Y + other.Y, Z + other.Z)
    }
}