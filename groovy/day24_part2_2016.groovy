
def content = new File("input.txt").text
def grid = content.readLines().collect { it as List }

def graph = []
grid.eachWithIndex { row, r ->
    row.eachWithIndex { cell, c ->
        if (cell =~ /[0-9]/) {
            def poi = cell
            def distancesFromPOI = bfsGetEdgeWeights(grid, [r, c])

            if (graph.isEmpty()) {
                distancesFromPOI.each { graph.add(new int[distancesFromPOI.size()]) }
            }
            def index = poi as int
            graph[index] = distancesFromPOI
        }
    }
}

println dfs(graph, 0, [0: true], true)

def bfsGetEdgeWeights(grid, start) {
    def poiToDistance = [(grid[start[0]][start[1]]): 0]
    def queue = [[start[0], start[1], 0] as int[]]
    def visited = [:].withDefault { false }

    while (queue) {
        def front = queue.remove(0)

        if (visited[[front[0], front[1]]]) continue
        visited[[front[0], front[1]]] = true

        if (grid[front[0]][front[1]] =~ /[0-9]/) {
            poiToDistance[grid[front[0]][front[1]]] = front[2]
        }
        
        [[0, -1], [0, 1], [1, 0], [-1, 0]].each { d ->
            def nextRow = front[0] + d[0]
            def nextCol = front[1] + d[1]
            
            if (grid[nextRow][nextCol] != "#") {
                queue.add([nextRow, nextCol, front[2] + 1] as int[])
            }
        }
    }

    def distances = new int[poiToDistance.size()]
    poiToDistance.each { numStr, dist ->
        def n = numStr as int
        distances[n] = dist
    }
    return distances
}

def dfs(graph, entryIndex, visited, returnToZero) {
    if (graph.size() == visited.size()) {
        if (returnToZero) {
            return graph[entryIndex][0]
        }
        return 0
    }

    def minDistance = Integer.MAX_VALUE
    graph[entryIndex].eachWithIndex { val, i ->
        if (!visited.containsKey(i)) {
            visited[i] = true

            def dist = val + dfs(graph, i, visited, returnToZero)
            minDistance = Math.min(minDistance, dist)

            visited.remove(i)
        }
    }

    return minDistance
}
