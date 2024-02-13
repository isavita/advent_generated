
import Foundation

struct Point: Hashable {
    var x: Int
    var y: Int
}

typealias DoorMap = [Point: Set<Point>]

let fileURL = URL(fileURLWithPath: "input.txt")
let regex = try String(contentsOf: fileURL)
let dm = buildMap(regex: String(regex.dropFirst().dropLast()))
let maxDoors = findFurthestRoom(dm: dm)
print(maxDoors)

func buildMap(regex: String) -> DoorMap {
    var dm = DoorMap()
    var stack = [Point]()
    var cp = Point(x: 0, y: 0)
    
    for c in regex {
        if c == "(" {
            stack.append(cp)
        } else if c == "|" {
            cp = stack.last!
        } else if c == ")" {
            cp = stack.removeLast()
        } else {
            let np = move(p: cp, dir: c)
            if dm[cp] == nil {
                dm[cp] = Set<Point>()
            }
            dm[cp]?.insert(np)
            cp = np
        }
    }
    
    return dm
}

func move(p: Point, dir: Character) -> Point {
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

func findFurthestRoom(dm: DoorMap) -> Int {
    var visited = [Point: Int]()
    var queue = [Point]()
    queue.append(Point(x: 0, y: 0))
    var maxDoors = 0
    
    while !queue.isEmpty {
        let p = queue.removeFirst()
        for np in dm[p] ?? Set<Point>() {
            if visited[np] == nil {
                visited[np] = visited[p, default: 0] + 1
                maxDoors = max(maxDoors, visited[np]!)
                queue.append(np)
            }
        }
    }
    
    return maxDoors
}

func max(_ a: Int, _ b: Int) -> Int {
    return a > b ? a : b
}
