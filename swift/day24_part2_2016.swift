
import Foundation

func cleaningRobot(input: String) -> Int {
    let grid = input.components(separatedBy: "\n").map { $0.map { String($0) } }
    var graph = [[Int]]()
    
    for r in 0..<grid.count {
        for c in 0..<grid[r].count {
            let cell = grid[r][c]
            if let _ = Int(cell) {
                let distancesFromPOI = bfsGetEdgeWeights(grid: grid, start: [r, c])
                
                if graph.isEmpty {
                    for _ in 0..<distancesFromPOI.count {
                        graph.append([Int](repeating: 0, count: distancesFromPOI.count))
                    }
                }
                let index = Int(cell)!
                graph[index] = distancesFromPOI
            }
        }
    }
    
    return dfs(graph: graph, entryIndex: 0, visited: [0: true], returnToZero: true)
}

func bfsGetEdgeWeights(grid: [[String]], start: [Int]) -> [Int] {
    var poiToDistance = [String: Int]()
    poiToDistance[grid[start[0]][start[1]]] = 0
    
    var queue = [(row: Int, col: Int, distance: Int)]()
    queue.append((start[0], start[1], 0))
    
    var visited = [Int: Bool]()
    
    while !queue.isEmpty {
        let front = queue.removeFirst()
        
        if visited[front.row * grid[0].count + front.col] != nil {
            continue
        }
        visited[front.row * grid[0].count + front.col] = true
        
        if let _ = Int(grid[front.row][front.col]) {
            poiToDistance[grid[front.row][front.col]] = front.distance
        }
        
        for d in dirs {
            let nextRow = front.row + d.0
            let nextCol = front.col + d.1
            
            if grid.indices.contains(nextRow) && grid[nextRow].indices.contains(nextCol) && grid[nextRow][nextCol] != "#" {
                queue.append((nextRow, nextCol, front.distance + 1))
            }
        }
    }
    
    var distances = [Int](repeating: 0, count: poiToDistance.count)
    for (numStr, dist) in poiToDistance {
        let n = Int(numStr)!
        distances[n] = dist
    }
    
    return distances
}

let dirs = [(0, -1), (0, 1), (1, 0), (-1, 0)]

func dfs(graph: [[Int]], entryIndex: Int, visited: [Int: Bool], returnToZero: Bool) -> Int {
    if graph.count == visited.count {
        if returnToZero {
            return graph[entryIndex][0]
        }
        return 0
    }
    
    var minDistance = Int.max
    for (i, val) in graph[entryIndex].enumerated() {
        if visited[i] == nil {
            var newVisited = visited
            newVisited[i] = true
            
            let dist = val + dfs(graph: graph, entryIndex: i, visited: newVisited, returnToZero: returnToZero)
            minDistance = min(minDistance, dist)
        }
    }
    
    return minDistance
}

func readFile(pathFromCaller: String) -> String {
    let filename = CommandLine.arguments[0]
    let absolutePath = URL(fileURLWithPath: filename).deletingLastPathComponent().appendingPathComponent(pathFromCaller)
    
    do {
        let content = try String(contentsOf: absolutePath)
        return content.trimmingCharacters(in: .newlines)
    } catch {
        fatalError(error.localizedDescription)
    }
}

let input = readFile(pathFromCaller: "input.txt")
let result = cleaningRobot(input: input)
print(result)
