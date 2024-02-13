
import Foundation

struct Point: Equatable, Hashable {
    var x: Int
    var y: Int
}

func isWall(favoriteNumber: Int, x: Int, y: Int) -> Bool {
    var num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
    var bits = 0
    while num > 0 {
        if num % 2 == 1 {
            bits += 1
        }
        num /= 2
    }
    return bits % 2 != 0
}

func bfs(start: Point, target: Point, favoriteNumber: Int) -> Int {
    var visited = Set<Point>()
    var queue = [start]
    var steps = 0
    
    while !queue.isEmpty {
        let size = queue.count
        for _ in 0..<size {
            let point = queue.removeFirst()
            if point == target {
                return steps
            }
            
            for delta in [Point(x: 1, y: 0), Point(x: -1, y: 0), Point(x: 0, y: 1), Point(x: 0, y: -1)] {
                let next = Point(x: point.x + delta.x, y: point.y + delta.y)
                if next.x >= 0 && next.y >= 0 && !isWall(favoriteNumber: favoriteNumber, x: next.x, y: next.y) && !visited.contains(next) {
                    visited.insert(next)
                    queue.append(next)
                }
            }
        }
        steps += 1
    }
    
    return -1
}

if let input = try? String(contentsOfFile: "input.txt") {
    let num = input.trimmingCharacters(in: .whitespacesAndNewlines)
    if let favoriteNumber = Int(num) {
        let start = Point(x: 1, y: 1)
        let target = Point(x: 31, y: 39)
        let steps = bfs(start: start, target: target, favoriteNumber: favoriteNumber)
        print(steps)
    }
}
