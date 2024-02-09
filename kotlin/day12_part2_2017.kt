import java.io.File

fun main(args: Array<String>) {
    val adj = mutableMapOf<Int, MutableList<Int>>()
    val visited = mutableMapOf<Int, Boolean>()
    var groups = 0

    File("input.txt").forEachLine { line ->
        val parts = line.split(" <-> ")
        val from = parts[0].toInt()
        val toNodes = parts[1].split(", ")

        adj.getOrPut(from) { mutableListOf() }
        toNodes.forEach { toNode ->
            val to = toNode.toInt()
            adj.getOrPut(to) { mutableListOf() }
            adj[from]!!.add(to)
            adj[to]!!.add(from)
        }
    }

    fun DFS(node: Int) {
        visited[node] = true
        adj[node]?.forEach { neighbor ->
            if (visited[neighbor] != true) {
                DFS(neighbor)
            }
        }
    }

    adj.keys.forEach { node ->
        if (visited[node] != true) {
            DFS(node)
            groups++
        }
    }

    println(groups)
}