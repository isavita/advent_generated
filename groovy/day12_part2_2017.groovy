def file = new File("input.txt")
def adj = [:]
file.eachLine { line ->
    def parts = line.split(" <-> ")
    def from = parts[0] as int
    def toNodes = parts[1].split(", ")
    toNodes.each { to ->
        def toNode = to as int
        if (!adj.containsKey(from)) adj[from] = []
        if (!adj.containsKey(toNode)) adj[toNode] = []
        adj[from] << toNode
        adj[toNode] << from
    }
}

def visited = [:]
def groups = 0

adj.keySet().each { node ->
    if (!visited.containsKey(node)) {
        def stack = [node]
        while (stack) {
            def n = stack.pop()
            if (!visited.containsKey(n)) {
                visited[n] = true
                stack.addAll(adj[n] - visited.keySet())
            }
        }
        groups++
    }
}

println groups