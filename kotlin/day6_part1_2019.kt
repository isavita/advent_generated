import java.io.File

fun countOrbits(orbitMap: Map<String, List<String>>, start: String, depth: Int): Int {
    val orbits = orbitMap[start] ?: return depth
    var count = depth
    for (orbit in orbits) {
        count += countOrbits(orbitMap, orbit, depth + 1)
    }
    return count
}

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val orbitMap = mutableMapOf<String, MutableList<String>>()
    for (line in lines) {
        val parts = line.split(")")
        val center = parts[0]
        val orbiter = parts[1]
        if (center !in orbitMap) {
            orbitMap[center] = mutableListOf()
        }
        orbitMap[center]!!.add(orbiter)
    }

    val totalOrbits = countOrbits(orbitMap, "COM", 0)
    println(totalOrbits)
}