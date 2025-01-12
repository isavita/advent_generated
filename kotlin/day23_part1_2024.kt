
import java.io.File
import java.util.*

fun main() {
    val graph = mutableMapOf<String, MutableSet<String>>()
    File("input.txt").forEachLine { line ->
        val computers = line.split("-")
        if (computers.size == 2) {
            val from = computers[0]
            val to = computers[1]
            graph.computeIfAbsent(from) { mutableSetOf() }.add(to)
            graph.computeIfAbsent(to) { mutableSetOf() }.add(from)
        }
    }

    val tripletsWithT = findTripletsWithT(graph)
    println("Number of triplets containing at least one computer with name starting with 't': ${tripletsWithT.size}")
}

fun findTripletsWithT(graph: Map<String, Set<String>>): Set<Set<String>> {
    val triplets = mutableSetOf<Set<String>>()
    val computers = graph.keys.toList()

    for (i in computers.indices) {
        for (j in i + 1 until computers.size) {
            for (k in j + 1 until computers.size) {
                val c1 = computers[i]
                val c2 = computers[j]
                val c3 = computers[k]

                if (c2 in graph[c1]!! && c3 in graph[c2]!! && c3 in graph[c1]!!) {
                    if (c1.startsWith("t") || c2.startsWith("t") || c3.startsWith("t")) {
                        triplets.add(setOf(c1, c2, c3))
                    }
                }
            }
        }
    }
    return triplets
}
