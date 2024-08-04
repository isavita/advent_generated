import java.io.File
import java.util.*

data class Edge(val from: String, val to: String, val distance: Int)

fun main() {
    val input = File("input.txt").readLines()
    val edges = mutableListOf<Edge>()

    // Parse the input file
    for (line in input) {
        val parts = line.split(" = ")
        val cities = parts[0].split(" to ")
        val distance = parts[1].toInt()
        edges.add(Edge(cities[0], cities[1], distance))
    }

    // Create a map to store the distances between cities
    val distanceMap = mutableMapOf<String, MutableMap<String, Int>>()
    for (edge in edges) {
        distanceMap.computeIfAbsent(edge.from) { mutableMapOf() }[edge.to] = edge.distance
        distanceMap.computeIfAbsent(edge.to) { mutableMapOf() }[edge.from] = edge.distance
    }

    // Get the list of cities
    val cities = distanceMap.keys.toList()

    // Find the shortest route
    var shortestDistance = Int.MAX_VALUE
    for (start in cities) {
        val remainingCities = cities.toMutableList()
        remainingCities.remove(start)
        shortestDistance = minOf(shortestDistance, findShortestRoute(start, remainingCities, distanceMap, 0))
    }

    println(shortestDistance)
}

fun findShortestRoute(current: String, remainingCities: List<String>, distanceMap: Map<String, Map<String, Int>>, currentDistance: Int): Int {
    if (remainingCities.isEmpty()) return currentDistance

    var shortestDistance = Int.MAX_VALUE
    for (next in remainingCities) {
        val distanceToNext = distanceMap[current]!![next]!!
        val newDistance = currentDistance + distanceToNext
        val newRemainingCities = remainingCities.toMutableList()
        newRemainingCities.remove(next)
        shortestDistance = minOf(shortestDistance, findShortestRoute(next, newRemainingCities, distanceMap, newDistance))
    }
    return shortestDistance
}