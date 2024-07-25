
class Edge {
    String start, end
    int weight = 1
}

def parseInput(input) {
    def graph = [:].withDefault { [:] }
    input.each { line ->
        def parts = line.split(": ")
        def vertice = parts[0]
        def others = parts[1].split(" ")
        others.each { other ->
            graph[vertice][other] = new Edge(start: vertice, end: other)
            graph[other][vertice] = new Edge(start: other, end: vertice)
        }
    }
    graph
}

def bfs(graph, start, goalFunc) {
    def frontier = [start] as Queue
    def reached = [(start): true]
    def cameFrom = [(start): start]

    while (!frontier.isEmpty()) {
        def current = frontier.poll()
        if (goalFunc(current)) return [true, cameFrom]

        graph[current]?.each { next ->
            if (!reached.containsKey(next.key)) {
                frontier.add(next.key)
                reached[next.key] = true
                cameFrom[next.key] = current
            }
        }
    }
    [false, cameFrom]
}

def reconstructPath(start, end, cameFrom) {
    def path = []
    def current = end
    while (current != start) {
        path.add(0, current)
        current = cameFrom[current]
    }
    path.add(0, start)
    path
}

def copyGraph(graph) {
    graph.collectEntries { vertice, edges ->
        [vertice, edges.collectEntries { edge -> [edge.key, edge.value] }]
    }
}

def solve(input) {
    def minCut = 3
    def graph = parseInput(input)
    def source = graph.keySet().first()
    def separateGraph

    graph.keySet().each { end ->
        if (source == end) return

        def newGraph = copyGraph(graph)
        (0..<minCut).each {
            def (found, cameFrom) = bfs(newGraph, source, { it == end })
            def path = reconstructPath(source, end, cameFrom)
            (0..<path.size() - 1).each { j ->
                newGraph[path[j]].remove(path[j + 1])
            }
        }

        if (!bfs(newGraph, source, { it == end })[0]) {
            separateGraph = newGraph
            return
        }
    }

    def (found, cameFrom) = bfs(separateGraph, source, { false })
    def length1 = cameFrom.size()
    def length2 = separateGraph.size() - length1
    length1 * length2
}

def readFile(fileName) {
    new File(fileName).readLines().collect { it.trim() }
}

def input = readFile("input.txt")
println solve(input)
