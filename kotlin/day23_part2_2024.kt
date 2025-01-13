
import java.io.File

val graph = mutableMapOf<String, MutableSet<String>>()
var bestClique = listOf<String>()

fun main() {
    val nodesSet = mutableSetOf<String>()
    File("input.txt").forEachLine { line ->
        val parts = line.split("-")
        if (parts.size == 2) {
            val (a, b) = parts
            graph.getOrPut(a) { mutableSetOf() }.add(b)
            graph.getOrPut(b) { mutableSetOf() }.add(a)
            nodesSet.add(a)
            nodesSet.add(b)
        }
    }

    val allNodes = nodesSet.toList()
    bronKerbosch(listOf(), allNodes, listOf())
    println(bestClique.sorted().joinToString(","))
}

fun bronKerbosch(R: List<String>, P: List<String>, X: List<String>) {
    if (P.isEmpty() && X.isEmpty()) {
        if (R.size > bestClique.size) {
            bestClique = R.toList()
        }
        return
    }

    val pCopy = P.toMutableList()
    for (v in P) {
        val neighbors = neighborsOf(v)
        bronKerbosch(
            R + v,
            pCopy.filter { it in neighbors },
            X.filter { it in neighbors }
        )
        pCopy.remove(v)
        
    }
}

fun neighborsOf(node: String): Set<String> = graph[node] ?: emptySet()
