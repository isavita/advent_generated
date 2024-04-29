def file = new File("input.txt")
def input = file.text.trim()

def graph = [:]
def parsed = input.split("\n").collect { it.split("-") }
parsed.each { pair ->
    if (!graph[pair[0]]) graph[pair[0]] = [:]
    if (!graph[pair[1]]) graph[pair[1]] = [:]
    graph[pair[0]][pair[1]] = true
    graph[pair[1]][pair[0]] = true
}

def walk(graph, current, visited, path, doubleUsed) {
    if (current == "end") return 1

    visited[current] = (visited[current] ?: 0) + 1

    def pathsToEnd = 0
    graph[current].each { visitable, _ ->
        if (visitable == "start") return
        if (visitable.toUpperCase() != visitable && (visited[visitable] ?: 0) > 0) {
            if (doubleUsed) return
            doubleUsed = true
        }

        path << visitable
        pathsToEnd += walk(graph, visitable, visited, path, doubleUsed)

        visited[visitable] = (visited[visitable] ?: 0) - 1
        path.pop()

        if (visitable.toUpperCase() != visitable && (visited[visitable] ?: 0) == 1) doubleUsed = false
    }

    pathsToEnd
}

def ans = walk(graph, "start", [:], ["start"], false)
println ans