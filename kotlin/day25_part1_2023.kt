import java.io.File

data class Vertice(val name: String)

data class Edge(val start: Vertice, val end: Vertice, val weight: Int)

typealias Graph = MutableMap<Vertice, MutableMap<Edge, Unit>>

fun parseInput(input: List<String>): Graph {
    val weight = 1
    val graph = mutableMapOf<Vertice, MutableMap<Edge, Unit>>()

    for (line in input) {
        val parts = line.split(": ")
        val vertice = Vertice(parts[0])
        val others = parts[1].split(" ")

        if (!graph.containsKey(vertice)) {
            graph[vertice] = mutableMapOf()
        }

        for (other in others) {
            val otherVertice = Vertice(other)
            if (!graph.containsKey(otherVertice)) {
                graph[otherVertice] = mutableMapOf()
            }

            graph[vertice]!![Edge(vertice, otherVertice, weight)] = Unit
            graph[otherVertice]!![Edge(otherVertice, vertice, weight)] = Unit
        }
    }

    return graph
}

fun breadthFirstSearch(graph: Graph, start: Vertice, goalFunc: (Vertice) -> Boolean): Pair<Boolean, Map<Vertice, Vertice>> {
    val frontier = mutableListOf(start)
    val reached = mutableMapOf(start to Unit)
    val cameFrom = mutableMapOf(start to start)

    while (frontier.isNotEmpty()) {
        val current = frontier.removeAt(0)

        if (goalFunc(current)) {
            return true to cameFrom
        }

        for (next in graph[current]!!.keys) {
            if (!reached.containsKey(next.end)) {
                frontier.add(next.end)
                reached[next.end] = Unit
                cameFrom[next.end] = current
            }
        }
    }

    return false to cameFrom
}

fun reconstructPath(start: Vertice, end: Vertice, cameFrom: Map<Vertice, Vertice>): List<Vertice> {
    val path = mutableListOf<Vertice>()
    var current = end
    while (current != start) {
        path.add(0, current)
        current = cameFrom[current]!!
    }
    path.add(0, start)
    return path
}

fun copyGraph(graph: Graph): Graph {
    val newGraph = mutableMapOf<Vertice, MutableMap<Edge, Unit>>()
    for ((vertice, edges) in graph) {
        newGraph[vertice] = mutableMapOf()
        for (edge in edges.keys) {
            newGraph[vertice]!![edge] = Unit
        }
    }
    return newGraph
}

fun solve(input: List<String>): Int {
    val minCut = 3
    val graph = parseInput(input)
    var source: Vertice? = null
    for (vertice in graph.keys) {
        source = vertice
        break
    }
    var separteGraph: Graph? = null
    for (end in graph.keys) {
        if (source == end) {
            continue
        }

        var newGraph = copyGraph(graph)
        for (i in 0 until minCut) {
            val (_, cameFrom) = breadthFirstSearch(newGraph, source!!, { it == end })
            val path = reconstructPath(source!!, end, cameFrom)
            for (j in 0 until path.size - 1) {
                val edge = Edge(path[j], path[j + 1], 1)
                newGraph[path[j]]!!.remove(edge)
            }
        }

        val (isValid, _) = breadthFirstSearch(newGraph, source!!, { it == end })
        if (!isValid) {
            separteGraph = newGraph
            break
        }
    }

    val (_, cameFrom) = breadthFirstSearch(separteGraph!!, source!!, { false })
    val length1 = cameFrom.size
    val length2 = separteGraph.size - length1

    return length1 * length2
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}