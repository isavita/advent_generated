import java.io.File

class Cave {
    val connections = mutableMapOf<String, Boolean>()

    fun connectTo(name: String) {
        connections[name] = true
    }

    fun disconnectFrom(name: String) {
        connections.remove(name)
    }
}

fun main(args: Array<String>) {
    val caves = mutableMapOf<String, Cave>()
    File("input.txt").forEachLine {
        val paths = it.split("-")
        val from = paths[0]
        val to = paths[1]

        if (!caves.containsKey(from)) {
            caves[from] = Cave()
        }

        if (!caves.containsKey(to)) {
            caves[to] = Cave()
        }

        caves[from]?.connectTo(to)
        caves[to]?.connectTo(from)
    }

    var count = 0
    fun dfs(current: String, visited: MutableMap<String, Boolean>) {
        if (current == "end") {
            count++
            return
        }

        caves[current]?.connections?.keys?.forEach { next ->
            if (visited[next] == true && next.toLowerCase() == next) {
                return@forEach
            }

            val visitedCopy = visited.toMutableMap()
            visitedCopy[next] = true
            dfs(next, visitedCopy)
        }
    }

    dfs("start", mutableMapOf("start" to true))
    println(count)
}