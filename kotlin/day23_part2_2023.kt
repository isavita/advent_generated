import java.io.File

data class Coord(val x: Int, val y: Int) {
    operator fun plus(other: Coord) = Coord(x + other.x, y + other.y)
}

class Grid(val width: Int, val height: Int, val data: MutableMap<Coord, Char>)

val North = Coord(0, -1)
val South = Coord(0, 1)
val West = Coord(-1, 0)
val East = Coord(1, 0)

const val Empty = '.'
const val Wall = '#'
const val NorthSlopes = '^'
const val SouthSlopes = 'v'
const val WestSlopes = '<'
const val EastSlopes = '>'

val SlopeToDir = mapOf(
    NorthSlopes to North,
    SouthSlopes to South,
    WestSlopes to West,
    EastSlopes to East
)

class Graph(val vertices: MutableSet<Coord>, val edges: MutableMap<Coord, MutableSet<Edge>>)

data class Edge(val start: Coord, val end: Coord, val weight: Int)

fun isInBounds(grid: Grid, coord: Coord) = coord.x in 0 until grid.width && coord.y in 0 until grid.height

fun parseInput(input: List<String>): Grid {
    val grid = Grid(input[0].length, input.size, mutableMapOf())
    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char != Empty) grid.data[Coord(x, y)] = char
        }
    }
    return grid
}

fun isValidNeighbor(grid: Grid, coord: Coord): Boolean {
    return isInBounds(grid, coord) && grid.data[coord] != Wall
}

fun isValidNeighborWithSlopes(grid: Grid, coord: Coord, dir: Coord): Boolean {
    return isInBounds(grid, coord) && (grid.data[coord] == null || grid.data[coord] != Wall && SlopeToDir[grid.data[coord]] == dir)
}

fun neighbors4(grid: Grid, coord: Coord, isValidNeighborFunc: (Grid, Coord) -> Boolean): List<Coord> {
    return listOf(North, South, West, East).map { coord + it }.filter { isValidNeighborFunc(grid, it) }
}

fun getGraph(grid: Grid, start: Coord, end: Coord, isValidNeighborFunc: (Grid, Coord) -> Boolean): Graph {
    val graph = Graph(mutableSetOf(start, end), mutableMapOf())
    for (y in 0 until grid.height) {
        for (x in 0 until grid.width) {
            val coord = Coord(x, y)
            if (grid.data[coord] == null && neighbors4(grid, coord, isValidNeighbor).size > 2) {
                graph.vertices.add(coord)
            }
        }
    }
    graph.vertices.forEach { vertex ->
        graph.edges[vertex] = getEdgesBFS(grid, vertex, graph.vertices, isValidNeighborFunc)
    }
    return graph
}

fun getEdgesBFS(grid: Grid, start: Coord, vertices: Set<Coord>, isValidNeighborFunc: (Grid, Coord) -> Boolean): MutableSet<Edge> {
    val frontier = mutableListOf(start)
    val reached = mutableSetOf(start)
    val distances = mutableMapOf(start to 0)
    val edges = mutableSetOf<Edge>()

    while (frontier.isNotEmpty()) {
        val current = frontier.removeAt(0)
        if (current in vertices && current != start) {
            edges.add(Edge(start, current, distances[current] ?: 0))
            continue
        }
        neighbors4(grid, current, isValidNeighborFunc).forEach { next ->
            if (next !in reached) {
                frontier.add(next)
                reached.add(next)
                distances[next] = distances[current]!! + 1
            }
        }
    }
    return edges
}

fun getMaxDistanceDFS(grid: Grid, graph: Graph, current: Coord, end: Coord, seen: MutableSet<Coord>): Pair<Boolean, Int> {
    if (current == end) return true to 0
    var maxi = 0
    seen.add(current)
    for (edge in graph.edges[current] ?: emptySet()) {
        if (edge.end !in seen) {
            val (isValid, dist) = getMaxDistanceDFS(grid, graph, edge.end, end, seen)
            if (isValid) maxi = maxOf(maxi, dist + edge.weight)
        }
    }
    seen.remove(current)
    return if (maxi == 0) false to 0 else true to maxi
}

fun solve(input: List<String>): Int {
    val grid = parseInput(input)
    val start = Coord(1, 0)
    val end = Coord(grid.width - 2, grid.height - 1)
    val graph = getGraph(grid, start, end, ::isValidNeighbor)
    val (_, maxDist) = getMaxDistanceDFS(grid, graph, start, end, mutableSetOf())
    return maxDist
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}