import java.io.File

fun main() {
    val distances = mutableMapOf<Pair<String, String>, Int>()
    val locations = mutableSetOf<String>()

    File("input.txt").readLines().forEach {
        val parts = it.split(" ")
        val location1 = parts[0]
        val location2 = parts[2]
        val distance = parts[4].toInt()

        distances[Pair(location1, location2)] = distance
        distances[Pair(location2, location1)] = distance
        locations.add(location1)
        locations.add(location2)
    }

    val permutations = locations.toList().permutations()
    var shortestDistance = Int.MAX_VALUE
    var longestDistance = Int.MIN_VALUE

    for (perm in permutations) {
        var totalDistance = 0
        for (i in 0 until perm.size - 1) {
            totalDistance += distances[Pair(perm[i], perm[i + 1])]!!
        }
        if (totalDistance < shortestDistance) {
            shortestDistance = totalDistance
        }
        if (totalDistance > longestDistance) {
            longestDistance = totalDistance
        }
    }

    println("Shortest Distance: $shortestDistance")
    println("Longest Distance: $longestDistance")
}

fun <T> List<T>.permutations(): List<List<T>> {
    if (size == 1) return listOf(this)
    val element = first()
    val perms = drop(1).permutations()
    return perms.flatMap { perm ->
        (0..perm.size).map { i ->
            perm.toMutableList().apply { add(i, element) }
        }
    }
}