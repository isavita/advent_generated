
import Foundation

struct Position: Hashable {
    let x: Int
    let y: Int
}

struct State: Hashable {
    let position: Position
    let keys: Set<Character>
}

func bfs(start: Position, grid: [[Character]], totalKeys: Int) -> Int {
    let directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    var queue: [(State, Int)] = [(State(position: start, keys: []), 0)]
    var visited: Set<State> = []
    
    while !queue.isEmpty {
        let (currentState, steps) = queue.removeFirst()
        
        if currentState.keys.count == totalKeys {
            return steps
        }
        
        for direction in directions {
            let newX = currentState.position.x + direction.0
            let newY = currentState.position.y + direction.1
            
            if newX < 0 || newY < 0 || newX >= grid.count || newY >= grid[0].count {
                continue
            }
            
            let cell = grid[newX][newY]
            var newKeys = currentState.keys
            
            if cell == "#" {
                continue // Wall
            } else if cell.isUppercase, !currentState.keys.contains(Character(String(cell).lowercased())) {
                continue // Door without key
            } else if cell.isLowercase {
                newKeys.insert(cell)
            }
            
            let newState = State(position: Position(x: newX, y: newY), keys: newKeys)
            
            if !visited.contains(newState) {
                visited.insert(newState)
                queue.append((newState, steps + 1))
            }
        }
    }
    
    return -1 // Should never reach here if all keys are reachable
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Failed to read input file.")
        return
    }
    
    let grid = input.split(separator: "\n").map { Array($0) }
    var start: Position?
    var totalKeys = 0
    
    for (i, row) in grid.enumerated() {
        for (j, cell) in row.enumerated() {
            if cell == "@" {
                start = Position(x: i, y: j)
            }
            if cell.isLowercase {
                totalKeys += 1
            }
        }
    }
    
    guard let startPosition = start else {
        print("No starting position found.")
        return
    }
    
    let result = bfs(start: startPosition, grid: grid, totalKeys: totalKeys)
    print("Minimum steps to collect all keys: \(result)")
}

main()
