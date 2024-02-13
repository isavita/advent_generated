
import Foundation

let favoriteNumber = 1362

struct Point: Hashable {
    var x: Int
    var y: Int
}

func isWall(x: Int, y: Int) -> Bool {
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

func bfsMaxSteps(start: Point, maxSteps: Int) -> Int {
    var visited: Set<Point> = []
    var queue: [Point] = [start]
    visited.insert(start)
    var steps = 0

    while !queue.isEmpty && steps < maxSteps {
        let size = queue.count
        for _ in 0..<size {
            let point = queue.removeFirst()

            for delta in [Point(x: 1, y: 0), Point(x: -1, y: 0), Point(x: 0, y: 1), Point(x: 0, y: -1)] {
                let next = Point(x: point.x + delta.x, y: point.y + delta.y)
                if next.x >= 0 && next.y >= 0 && !isWall(x: next.x, y: next.y) && !visited.contains(next) {
                    visited.insert(next)
                    queue.append(next)
                }
            }
        }
        steps += 1
    }

    return visited.count
}

if let input = try? String(contentsOfFile: "input.txt") {
    let start = Point(x: 1, y: 1)
    let reachableLocations = bfsMaxSteps(start: start, maxSteps: 50)
    print(reachableLocations)
}
