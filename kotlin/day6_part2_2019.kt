import java.io.File

fun main() {
    val orbits = mutableMapOf<String, String>()
    File("input.txt").readLines().forEach {
        val (center, orbiter) = it.split(")")
        orbits[orbiter] = center
    }

    var totalOrbits = 0
    orbits.keys.forEach { orbiter ->
        var current = orbiter
        while (orbits.containsKey(current)) {
            current = orbits[current]!!
            totalOrbits++
        }
    }

    println(totalOrbits) // Part One: 160040

    val youPath = mutableListOf<String>()
    var current = "YOU"
    while (orbits.containsKey(current)) {
        current = orbits[current]!!
        youPath.add(current)
    }

    val sanPath = mutableListOf<String>()
    current = "SAN"
    while (orbits.containsKey(current)) {
        current = orbits[current]!!
        sanPath.add(current)
    }

    val commonAncestor = youPath.intersect(sanPath).first()
    val transfers = youPath.indexOf(commonAncestor) + sanPath.indexOf(commonAncestor)

    println(transfers) // Part Two: 373
}