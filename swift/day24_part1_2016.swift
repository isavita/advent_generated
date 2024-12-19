
import Foundation

func cleaningRobot(input: String) -> Int {
    let grid = input.split(separator: "\n").map { Array($0) }
    var graph: [[Int]] = []
    
    for r in 0..<grid.count {
        for c in 0..<grid[r].count {
            if grid[r][c].isNumber {
                let poi = String(grid[r][c])
                let distancesFromPOI = bfsGetEdgeWeights(grid: grid, start: (r, c))
                
                if graph.isEmpty {
                    graph = Array(repeating: Array(repeating: 0, count: distancesFromPOI.count), count: distancesFromPOI.count)
                }
                
                if let index = Int(poi) {
                    graph[index] = distancesFromPOI
                }
            }
        }
    }
    
    return dfs(graph: graph, entryIndex: 0, visited: [0: true], returnToZero: false)
}

struct BFSNode {
    let row: Int
    let col: Int
    let distance: Int
}

func bfsGetEdgeWeights(grid: [[Character]], start: (Int, Int)) -> [Int] {
    var poiToDistance: [String: Int] = [String(grid[start.0][start.1]): 0]
    var queue = [BFSNode(row: start.0, col: start.1, distance: 0)]
    var visited: Set<[Int]> = []
    
    let dirs = [[0, -1], [0, 1], [1, 0], [-1, 0]]
    
    while !queue.isEmpty {
        let front = queue.removeFirst()
        
        if visited.contains([front.row, front.col]) {
            continue
        }
        visited.insert([front.row, front.col])
        
        if grid[front.row][front.col].isNumber {
            poiToDistance[String(grid[front.row][front.col])] = front.distance
        }
        
        for d in dirs {
            let nextRow = front.row + d[0]
            let nextCol = front.col + d[1]
            
            if nextRow >= 0 && nextRow < grid.count && nextCol >= 0 && nextCol < grid[0].count && grid[nextRow][nextCol] != "#" {
                queue.append(BFSNode(row: nextRow, col: nextCol, distance: front.distance + 1))
            }
        }
    }
    
    var distances = Array(repeating: 0, count: poiToDistance.count)
    for (numStr, dist) in poiToDistance {
        if let n = Int(numStr) {
            distances[n] = dist
        }
    }
    return distances
}

func dfs(graph: [[Int]], entryIndex: Int, visited: [Int: Bool], returnToZero: Bool) -> Int {
    if graph.count == visited.count {
        return returnToZero ? graph[entryIndex][0] : 0
    }
    
    var minDistance = Int.max
    for (i, val) in graph[entryIndex].enumerated() {
        if visited[i] == nil {
            var newVisited = visited
            newVisited[i] = true
            minDistance = min(minDistance, val + dfs(graph: graph, entryIndex: i, visited: newVisited, returnToZero: returnToZero))
        }
    }
    
    return minDistance
}

func readFile(atPath path: String) -> String? {
    do {
        let fileURL = URL(fileURLWithPath: path)
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        return content.trimmingCharacters(in: .whitespacesAndNewlines)
    } catch {
        return nil
    }
}

if let input = readFile(atPath: "input.txt") {
    print(cleaningRobot(input: input))
}
