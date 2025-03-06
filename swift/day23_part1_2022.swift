
import Foundation

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Could not read input file")
    }

    var elves = Set<Point>()
    input.split(separator: "\n").enumerated().forEach { y, line in
        line.enumerated().forEach { x, char in
            if char == "#" {
                elves.insert(Point(x: x, y: y))
            }
        }
    }

    var directions: [Direction] = [.north, .south, .west, .east]

    for _ in 0..<10 {
        var proposedMoves = [Point: [Point]]()
        var elfMoved = false

        for elf in elves {
            // Check if the Elf needs to move
            if elf.neighbors8().allSatisfy({ !elves.contains($0) }) {
                continue
            }

            for direction in directions {
                let neighbors = elf.neighbors(for: direction)
                if neighbors.allSatisfy({ !elves.contains($0) }) {
                    let proposedMove = elf + direction.offset
                    proposedMoves[proposedMove, default: []].append(elf)
                    break
                }
            }
        }

        for (proposedMove, proposingElves) in proposedMoves {
            if proposingElves.count == 1 {
                elves.remove(proposingElves[0])
                elves.insert(proposedMove)
                elfMoved = true
            }
        }
      
        // Rotate directions
        directions.append(directions.removeFirst())
    }

    let minX = elves.min(by: { $0.x < $1.x })!.x
    let maxX = elves.max(by: { $0.x < $1.x })!.x
    let minY = elves.min(by: { $0.y < $1.y })!.y
    let maxY = elves.max(by: { $0.y < $1.y })!.y

    let area = (maxX - minX + 1) * (maxY - minY + 1)
    let emptyTiles = area - elves.count
    print(emptyTiles)
}

struct Point: Hashable {
    let x: Int
    let y: Int

    func neighbors8() -> [Point] {
        var result = [Point]()
        for dy in -1...1 {
            for dx in -1...1 {
                if dx != 0 || dy != 0 {
                    result.append(Point(x: x + dx, y: y + dy))
                }
            }
        }
        return result
    }
  
    func neighbors(for direction: Direction) -> [Point] {
        switch direction {
        case .north:
            return [Point(x: x - 1, y: y - 1), Point(x: x, y: y - 1), Point(x: x + 1, y: y - 1)]
        case .south:
            return [Point(x: x - 1, y: y + 1), Point(x: x, y: y + 1), Point(x: x + 1, y: y + 1)]
        case .west:
            return [Point(x: x - 1, y: y - 1), Point(x: x - 1, y: y), Point(x: x - 1, y: y + 1)]
        case .east:
            return [Point(x: x + 1, y: y - 1), Point(x: x + 1, y: y), Point(x: x + 1, y: y + 1)]
        }
    }

    static func + (left: Point, right: Point) -> Point {
        return Point(x: left.x + right.x, y: left.y + right.y)
    }
}

enum Direction {
    case north, south, west, east

    var offset: Point {
        switch self {
        case .north: return Point(x: 0, y: -1)
        case .south: return Point(x: 0, y: 1)
        case .west: return Point(x: -1, y: 0)
        case .east: return Point(x: 1, y: 0)
        }
    }
}

main()
