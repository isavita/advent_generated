import java.io.File

fun DFS(node: Int, adj: Map<Int, List<Int>>, visited: MutableMap<Int, Boolean>) {
    visited[node] = true
    adj[node]?.forEach { neighbor ->
        if (visited[neighbor] != true) {
            DFS(neighbor, adj, visited)
        }
    }
}

fun main(args: Array<String>) {
    val file = File("input.txt")
    val adj = mutableMapOf<Int, MutableList<Int>>()
    val visited = mutableMapOf<Int, Boolean>()

    file.forEachLine { line ->
        val parts = line.split(" <-> ")
        val from = parts[0].toInt()
        val toNodes = parts[1].split(", ")

        toNodes.forEach { toNode ->
            val to = toNode.toInt()
            adj.getOrPut(from) { mutableListOf() }.add(to)
            adj.getOrPut(to) { mutableListOf() }.add(from)
        }
    }

    DFS(0, adj, visited)

    val count = visited.count { it.value }
    println(count)
}