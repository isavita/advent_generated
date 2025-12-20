import java.io.File

private val nameToIdx = HashMap<String, Int>()
private val adj = mutableListOf<MutableList<Int>>()

private fun idx(name: String): Int {
    return nameToIdx.getOrPut(name) {
        adj.add(mutableListOf())
        adj.size - 1
    }
}

private fun dfs(cur: Int, tgt: Int, memo: LongArray): Long {
    if (cur == tgt) return 1L
    val cached = memo[cur]
    if (cached != -1L) return cached
    var sum = 0L
    for (v in adj[cur]) sum += dfs(v, tgt, memo)
    memo[cur] = sum
    return sum
}

private fun countPaths(s: Int, t: Int): Long {
    val memo = LongArray(adj.size) { -1L }
    return dfs(s, t, memo)
}

fun main() {
    File("input.txt").forEachLine { lineRaw ->
        var line = lineRaw.trim()
        if (line.isEmpty()) return@forEachLine
        val colonIdx = line.indexOf(':')
        if (colonIdx == -1) return@forEachLine
        val src = line.substring(0, colonIdx).trim()
        val rest = line.substring(colonIdx + 1)
        val u = idx(src)
        for (tok in rest.split(Regex("\\s+")).filter { it.isNotEmpty() }) {
            val v = idx(tok)
            adj[u].add(v)
        }
    }

    val svr = idx("svr")
    val dac = idx("dac")
    val fft = idx("fft")
    val out = idx("out")

    val s1 = countPaths(svr, dac) * countPaths(dac, fft) * countPaths(fft, out)
    val s2 = countPaths(svr, fft) * countPaths(fft, dac) * countPaths(dac, out)

    println("Paths (svr->dac->fft->out): $s1")
    println("Paths (svr->fft->dac->out): $s2")
    println("Total paths visiting both: ${s1 + s2}")
}