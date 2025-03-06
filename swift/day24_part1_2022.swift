
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int

    func moved(dx: Int, dy: Int) -> Point {
        return Point(x: x + dx, y: y + dy)
    }
}

enum Direction: Character {
    case up = "^"
    case down = "v"
    case left = "<"
    case right = ">"

    var dx: Int {
        switch self {
        case .up: return 0
        case .down: return 0
        case .left: return -1
        case .right: return 1
        }
    }

    var dy: Int {
        switch self {
        case .up: return -1
        case .down: return 1
        case .left: return 0
        case .right: return 0
        }
    }
}

struct Blizzard {
    var position: Point
    let direction: Direction
}

struct State: Hashable {
    let position: Point
    let time: Int
}

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Could not read input.txt")
    }

    let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n")
    var blizzards: [Blizzard] = []
    var walls: Set<Point> = []

    for (y, line) in lines.enumerated() {
        for (x, char) in line.enumerated() {
            let point = Point(x: x, y: y)
            if char == "#" {
                walls.insert(point)
            } else if let direction = Direction(rawValue: char) {
                blizzards.append(Blizzard(position: point, direction: direction))
            }
        }
    }

    let start = Point(x: 1, y: 0)
    let end = Point(x: lines[0].count - 2, y: lines.count - 1)
    let width = lines[0].count
    let height = lines.count

    func blizzardPositions(at time: Int) -> Set<Point> {
        var positions: Set<Point> = []
        for blizzard in blizzards {
            var x = blizzard.position.x + blizzard.direction.dx * time
            var y = blizzard.position.y + blizzard.direction.dy * time

            while x <= 0 { x += width - 2 }
            while x >= width - 1 { x -= width - 2 }
            while y <= 0 { y += height - 2 }
            while y >= height - 1 { y -= height - 2}

            positions.insert(Point(x: x, y: y))
        }
        return positions
    }
    
    func bfs() -> Int {
            var queue: [State] = [State(position: start, time: 0)]
            var visited: Set<State> = [State(position: start, time: 0)]
            
            while !queue.isEmpty {
                let currentState = queue.removeFirst()
                
                if currentState.position == end {
                    return currentState.time
                }
                
                let nextTime = currentState.time + 1
                let nextBlizzardPositions = blizzardPositions(at: nextTime)
                
                let moves = [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)]
                
                for move in moves {
                    let nextPosition = currentState.position.moved(dx: move.0, dy: move.1)
                    let nextState = State(position: nextPosition, time: nextTime)
                    
                    if !walls.contains(nextPosition) && !nextBlizzardPositions.contains(nextPosition) &&
                        nextPosition.x >= 0 && nextPosition.x < width && nextPosition.y >= 0 && nextPosition.y < height &&
                        !visited.contains(nextState) {
                        
                        queue.append(nextState)
                        visited.insert(nextState)
                    }
                }
        }
            return -1
        }

        let minTime = bfs()
        print(minTime)
}

solve()
