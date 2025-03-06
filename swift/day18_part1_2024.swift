
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int

    func neighbors() -> [Point] {
        return [
            Point(x: x - 1, y: y),
            Point(x: x + 1, y: y),
            Point(x: x, y: y - 1),
            Point(x: x, y: y + 1),
        ]
    }
}

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Error reading input file")
        return
    }

    let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
    var bytePositions: [Point] = []
    for line in lines {
        let coordinates = line.components(separatedBy: ",").compactMap { Int($0) }
        if coordinates.count == 2 {
            bytePositions.append(Point(x: coordinates[0], y: coordinates[1]))
        }
    }

    let gridSize = 71 // 0 to 70 inclusive
    let maxBytes = min(1024, bytePositions.count) // Simulate first 1024 bytes

    var corrupted: Set<Point> = []
    
    // No need to simulate steps; add corrupted locations directly
    for i in 0..<maxBytes {
         corrupted.insert(bytePositions[i])
    }


    func isValid(_ point: Point) -> Bool {
        return point.x >= 0 && point.x < gridSize && point.y >= 0 && point.y < gridSize && !corrupted.contains(point)
    }

    func bfs() -> Int? {
        var queue: [(Point, Int)] = [(Point(x: 0, y: 0), 0)]
        var visited: Set<Point> = [Point(x: 0, y: 0)]

        while !queue.isEmpty {
            let (currentPoint, steps) = queue.removeFirst()

            if currentPoint.x == gridSize - 1 && currentPoint.y == gridSize - 1 {
                return steps
            }

            for neighbor in currentPoint.neighbors() {
                if isValid(neighbor) && !visited.contains(neighbor) {
                    visited.insert(neighbor)
                    queue.append((neighbor, steps + 1))
                }
            }
        }

        return nil // No path found
    }

    if let minSteps = bfs() {
        print(minSteps)
    } else {
        print("No path found")
    }
}


// Ensure the program has a proper main entry point
solve()

