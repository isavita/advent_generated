
import java.io.File

fun main() {
    val nameToIdx = HashMap<String, Int>()
    val adj = mutableListOf<MutableList<Int>>()
    fun idx(name: String): Int =
        nameToIdx.getOrPut(name) { adj.add(mutableListOf()); adj.size - 1 }

    File("input.txt").forEachLine { line ->
        val parts = line.split(":")
        if (parts.size < 2) return@forEachLine
        val u = idx(parts[0].trim())
        val tokens = parts[1].trim().split(Regex("\\s+")).filter { it.isNotEmpty() }
        for (t in tokens) adj[u].add(idx(t))
    }

    val start = idx("you")
    val end = idx("out")
    val memo = IntArray(adj.size) { -1 }
    fun dfs(u: Int): Int {
        if (u == end) return 1
        val cached = memo[u]
        if (cached != -1) return cached
        var total = 0
        for (v in adj[u]) total += dfs(v)
        memo[u] = total
        return total
    }

    println(dfs(start))
}
