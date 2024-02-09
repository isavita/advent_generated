import java.io.File

data class Node(val used: Int, val avail: Int)

fun main(args: Array<String>) {
    val nodes = readNodes("input.txt")
    val viablePairs = countViablePairs(nodes)
    println(viablePairs)
}

fun readNodes(filename: String): List<Node> {
    val nodes = mutableListOf<Node>()
    File(filename).forEachLine {
        val nodeRegex = Regex("""node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%""")
        val matches = nodeRegex.find(it)
        if (matches != null) {
            val (used, avail) = matches.destructured
            nodes.add(Node(used.toInt(), avail.toInt()))
        }
    }
    return nodes
}

fun countViablePairs(nodes: List<Node>): Int {
    var count = 0
    for (i in nodes.indices) {
        for (j in nodes.indices) {
            if (i != j && nodes[i].used > 0 && nodes[i].used <= nodes[j].avail) {
                count++
            }
        }
    }
    return count
}