import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val cubes = input.map { it.split(",") }.map { (x, y, z) -> Cube(x.toInt(), y.toInt(), z.toInt()) }.toSet()

    val surfaceArea = cubes.sumOf { cube ->
        6 - cube.adjacentCubes(cubes).size
    }

    println(surfaceArea)
}

data class Cube(val x: Int, val y: Int, val z: Int) {
    fun adjacentCubes(cubes: Set<Cube>): Set<Cube> {
        val adjacent = mutableSetOf<Cube>()
        if (cubes.contains(Cube(x - 1, y, z))) adjacent.add(Cube(x - 1, y, z))
        if (cubes.contains(Cube(x + 1, y, z))) adjacent.add(Cube(x + 1, y, z))
        if (cubes.contains(Cube(x, y - 1, z))) adjacent.add(Cube(x, y - 1, z))
        if (cubes.contains(Cube(x, y + 1, z))) adjacent.add(Cube(x, y + 1, z))
        if (cubes.contains(Cube(x, y, z - 1))) adjacent.add(Cube(x, y, z - 1))
        if (cubes.contains(Cube(x, y, z + 1))) adjacent.add(Cube(x, y, z + 1))
        return adjacent
    }
}