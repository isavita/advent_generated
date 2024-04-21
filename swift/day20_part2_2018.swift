import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }
    
    static func == (lhs: Point, rhs: Point) -> Bool {
        return lhs.x == rhs.x && lhs.y == rhs.y
    }
}

typealias DoorMap = [Point: [Point: Bool]]

func buildMap(from regex: String) -> DoorMap {
    var dm: DoorMap = [:]
    var stack: [Point] = []
    var cp = Point(x: 0, y: 0)
    for c in regex {
        if c == "(" {
            stack.append(cp)
        } else if c == "|" {
            cp = stack.last!
        } else if c == ")" {
            cp = stack.last!
            stack.removeLast()
        } else {
            let np = move(cp, dir: c)
            if dm[cp] == nil {
                dm[cp] = [:]
            }
            dm[cp]?[np] = true
            cp = np
        }
    }
    return dm
}

func move(_ p: Point, dir: Character) -> Point {
    switch dir {
    case "N":
        return Point(x: p.x, y: p.y - 1)
    case "S":
        return Point(x: p.x, y: p.y + 1)
    case "E":
        return Point(x: p.x + 1, y: p.y)
    case "W":
        return Point(x: p.x - 1, y: p.y)
    default:
        return p
    }
}

func countRooms(_ dm: DoorMap, minDoors: Int) -> Int {
    var visited: [Point: Int] = [:]
    var queue: [Point] = [Point(x: 0, y: 0)]
    var roomCount = 0
    
    while !queue.isEmpty {
        let p = queue.removeFirst()
        for np in dm[p] ?? [:] {
            if visited[np.key] == nil {
                visited[np.key] = (visited[p] ?? 0) + 1
                if visited[np.key]! >= minDoors {
                    roomCount += 1
                }
                queue.append(np.key)
            }
        }
    }
    return roomCount
}

if let data = try? String(contentsOfFile: "input.txt") {
    let regex = String(data.dropFirst().dropLast())
    let dm = buildMap(from: regex)
    print(countRooms(dm, minDoors: 1000))
} else {
    print("Error reading file")
}