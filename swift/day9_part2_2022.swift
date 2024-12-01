
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int

    func add(_ other: Point) -> Point {
        return Point(x: x + other.x, y: y + other.y)
    }
}

enum Direction: CaseIterable {
    case north, east, south, west

    var point: Point {
        switch self {
        case .north: return Point(x: 0, y: 1)
        case .east: return Point(x: 1, y: 0)
        case .south: return Point(x: 0, y: -1)
        case .west: return Point(x: -1, y: 0)
        }
    }
}

func abs(_ x: Int) -> Int {
    return x < 0 ? -x : x
}

func sign(_ n: Int) -> Int {
    switch n {
    case 0: return 0
    case let n where n < 0: return -1
    default: return 1
    }
}

func next(_ head: Point, _ tail: Point) -> Point {
    let dx = head.x - tail.x
    let dy = head.y - tail.y
    if abs(dx) <= 1 && abs(dy) <= 1 { return tail }
    return tail.add(Point(x: sign(dx), y: sign(dy)))
}

func visited(_ input: String, _ ropeLength: Int) -> Int {
    var rope = Array(repeating: Point(x: 0, y: 0), count: ropeLength)
    var visited = Set<Point>()
    visited.insert(rope.last!)

    for line in input.components(separatedBy: "\n") {
        var parts = line.split(separator: " ")
        guard let directionChar = parts.first, let steps = Int(parts.last ?? "0") else { continue }
        let direction: Direction = {
            switch directionChar.lowercased() {
            case "u", "n": return .north
            case "r", "e": return .east
            case "d", "s": return .south
            case "l", "w": return .west
            default: return .north // Handle invalid input
            }
        }()
        let directionPoint = direction.point

        for _ in 0..<steps {
            rope[0] = rope[0].add(directionPoint)
            for i in 1..<ropeLength {
                rope[i] = next(rope[i-1], rope[i])
            }
            visited.insert(rope.last!)
        }
    }
    return visited.count
}

func main() {
    do {
        let input = try String(contentsOfFile: "input.txt")
        print(visited(input, 10))
    } catch {
        print("Error reading file: \(error)")
    }
}

main()

