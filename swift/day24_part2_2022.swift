
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
}

struct Blizzard {
    let pos: Point
    let dir: Direction
}

func readInput(from file: String) -> ([Blizzard], Int, Int) {
    guard let contents = try? String(contentsOfFile: file) else {
        fatalError("Could not read file \(file)")
    }

    let lines = contents.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n")
    var blizzards: [Blizzard] = []
    let height = lines.count
    let width = lines[0].count

    for (y, line) in lines.enumerated() {
        for (x, char) in line.enumerated() {
            if let dir = Direction(rawValue: char) {
                blizzards.append(Blizzard(pos: Point(x: x, y: y), dir: dir))
            }
        }
    }

    return (blizzards, width, height)
}

func blizzardPositions(at time: Int, blizzards: [Blizzard], width: Int, height: Int) -> Set<Point> {
    var positions: Set<Point> = []

    for blizzard in blizzards {
        var x = blizzard.pos.x
        var y = blizzard.pos.y

        switch blizzard.dir {
        case .up:
            y = (blizzard.pos.y - 1 - time) %% (height - 2) + 1
        case .down:
            y = (blizzard.pos.y - 1 + time) %% (height - 2) + 1
        case .left:
            x = (blizzard.pos.x - 1 - time) %% (width - 2) + 1
        case .right:
            x = (blizzard.pos.x - 1 + time) %% (width - 2) + 1
        }
        positions.insert(Point(x: x, y: y))
    }
    return positions
}


func solve(blizzards: [Blizzard], width: Int, height: Int, start: Point, end: Point, initialTime: Int) -> Int {
    var queue: [(Point, Int)] = [(start, initialTime)]
    var visited: Set<[Int]> = [[start.x, start.y, initialTime]]
    var minTime = Int.max

    while !queue.isEmpty {
        let (currentPos, currentTime) = queue.removeFirst()

        if currentTime >= minTime {
            continue
        }

        if currentPos == end {
            minTime = min(minTime, currentTime)
            continue
        }

        let nextTime = currentTime + 1
        let nextBlizzardPositions = blizzardPositions(at: nextTime, blizzards: blizzards, width: width, height: height)

        let moves = [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)]  // Wait, down, up, right, left

        for (dx, dy) in moves {
            let nextPos = currentPos.moved(dx: dx, dy: dy)

            if (nextPos == start || nextPos == end || (nextPos.x > 0 && nextPos.x < width - 1 && nextPos.y > 0 && nextPos.y < height - 1)) &&
                !nextBlizzardPositions.contains(nextPos) {
                if !visited.contains([nextPos.x, nextPos.y, nextTime %% ((width-2) * (height-2))]) {
                    visited.insert([nextPos.x, nextPos.y, nextTime %% ((width-2) * (height-2))])
                    queue.append((nextPos, nextTime))
                }
            }
        }
    }
    return minTime
}

precedencegroup ExponentiationPrecedence {
    associativity: right
    higherThan: MultiplicationPrecedence
}

infix operator %% : ExponentiationPrecedence

func %% (lhs: Int, rhs: Int) -> Int {
    let mod = lhs % rhs
    return mod >= 0 ? mod : mod + rhs
}


func main() {
    let (blizzards, width, height) = readInput(from: "input.txt")
    let start = Point(x: 1, y: 0)
    let end = Point(x: width - 2, y: height - 1)
    
    let time1 = solve(blizzards: blizzards, width: width, height: height, start: start, end: end, initialTime: 0)
    print("Part 1: \(time1)")
    
    let time2 = solve(blizzards: blizzards, width: width, height: height, start: end, end: start, initialTime: time1)
    let time3 = solve(blizzards: blizzards, width: width, height: height, start: start, end: end, initialTime: time2)

    print("Part 2: \(time3)")
}


main()
