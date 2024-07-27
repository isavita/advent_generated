
import Foundation

struct Position: Comparable {
    let x: Int
    let y: Int
    let risk: Int

    static func < (lhs: Position, rhs: Position) -> Bool {
        return lhs.risk < rhs.risk
    }
}

func dijkstra(_ grid: [[Int]]) -> Int {
    var pq = [Position(x: 0, y: 0, risk: 0)]
    var dist = Array(repeating: Array(repeating: Int.max, count: grid[0].count), count: grid.count)
    dist[0][0] = 0

    let directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]

    while !pq.isEmpty {
        pq.sort() // Min-heap behavior
        let curr = pq.removeFirst()
        
        if curr.x == grid.count - 1 && curr.y == grid[0].count - 1 {
            return curr.risk
        }
        
        for (dx, dy) in directions {
            let nx = curr.x + dx
            let ny = curr.y + dy
            if nx >= 0 && ny >= 0 && nx < grid.count && ny < grid[0].count {
                let nextRisk = curr.risk + grid[nx][ny]
                if nextRisk < dist[nx][ny] {
                    dist[nx][ny] = nextRisk
                    pq.append(Position(x: nx, y: ny, risk: nextRisk))
                }
            }
        }
    }
    return -1
}

func readInput(from file: String) -> [[Int]] {
    let url = URL(fileURLWithPath: file)
    let content = try! String(contentsOf: url)
    return content.split(separator: "\n").map { Array($0).map { Int(String($0))! } }
}

let grid = readInput(from: "input.txt")
print(dijkstra(grid))
