
import Foundation

struct Position: Hashable {
    let row: Int
    let col: Int
    let direction: Int // 0: East, 1: South, 2: West, 3: North
}

struct State {
    let position: Position
    let score: Int
}

func solveMaze(maze: [[Character]]) -> Int {
    let rows = maze.count
    let cols = maze[0].count
    var start: Position?
    var end: Position?

    for r in 0..<rows {
        for c in 0..<cols {
            if maze[r][c] == "S" {
                start = Position(row: r, col: c, direction: 0) // Start facing East
            } else if maze[r][c] == "E" {
                end = Position(row: r, col: c, direction: 0) // Direction doesn't matter for the end
            }
        }
    }

    guard let startPosition = start, let endPosition = end else {
        return -1 // Or handle the error appropriately
    }

    var visited: Set<Position> = []
    var queue: [State] = [State(position: startPosition, score: 0)]
    var bestScore = Int.max

    let directions = [(0, 1), (1, 0), (0, -1), (-1, 0)] // E, S, W, N

    while !queue.isEmpty {
        let currentState = queue.removeFirst()
        let currentPosition = currentState.position
        let currentScore = currentState.score

        if currentPosition.row == endPosition.row && currentPosition.col == endPosition.col {
            bestScore = min(bestScore, currentScore)
            continue
        }

        if visited.contains(currentPosition) {
            continue
        }
        visited.insert(currentPosition)
        
        // Move forward
        let dr = directions[currentPosition.direction].0
        let dc = directions[currentPosition.direction].1
        let newRow = currentPosition.row + dr
        let newCol = currentPosition.col + dc

        if newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols && maze[newRow][newCol] != "#" {
            let newPos = Position(row: newRow, col: newCol, direction: currentPosition.direction)
            queue.append(State(position: newPos, score: currentScore + 1))
            
            // Optimization: Sort the queue based on a heuristic (Manhattan distance + turn penalty)
            queue.sort {
                let heuristic1 = abs($0.position.row - endPosition.row) + abs($0.position.col - endPosition.col) + ($0.position.direction != endPosition.direction ? 1000 : 0) + $0.score
                let heuristic2 = abs($1.position.row - endPosition.row) + abs($1.position.col - endPosition.col) + ($1.position.direction != endPosition.direction ? 1000 : 0) + $1.score
                
                return heuristic1 < heuristic2
            }
        }

        // Rotate clockwise
        let newDirectionCW = (currentPosition.direction + 1) % 4
        let newPositionCW = Position(row: currentPosition.row, col: currentPosition.col, direction: newDirectionCW)
         queue.append(State(position: newPositionCW, score: currentScore + 1000))

        // Rotate counter-clockwise
        let newDirectionCCW = (currentPosition.direction + 3) % 4 // or (currentPosition.direction - 1 + 4) % 4
        let newPositionCCW = Position(row: currentPosition.row, col: currentPosition.col, direction: newDirectionCCW)
        queue.append(State(position: newPositionCCW, score: currentScore + 1000))
    }

    return bestScore
}


func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = input.split(separator: "\n").map { String($0) }
        let maze = lines.map { Array($0) }
        
        let lowestScore = solveMaze(maze: maze)
        print(lowestScore)

    } catch {
        print("Error reading file: \(error)")
    }
}


main()
