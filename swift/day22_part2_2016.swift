
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
}

struct Node {
    let used: Int
    let avail: Int
}

let neighbors4 = [Point(x: 0, y: 1), Point(x: 0, y: -1), Point(x: 1, y: 0), Point(x: -1, y: 0)]

func dim(_ nodes: [Point: Node]) -> (Int, Int) {
    var w = 0
    var h = 0
    for p in nodes.keys {
        w = max(w, p.x)
        h = max(h, p.y)
    }
    return (w, h)
}

func findHole(_ nodes: [Point: Node]) -> Point {
    for (p, n) in nodes {
        if n.used == 0 {
            return p
        }
    }
    fatalError("no hole")
}
func moves(_ nodes: [Point: Node], _ goal: Point, _ fromPos: Point, _ toPos: Point) -> Int {
    let (w, h) = dim(nodes)
    var depth: [Point: Int] = [fromPos: 0]
    var pq: [(Int, Point)] = [(0, fromPos)]

    while !pq.isEmpty {
        pq.sort { $0.0 < $1.0 }
        let (currDepth, p) = pq.removeFirst()

        if p == toPos {
            return currDepth
        }

        let nextDepth = depth[p]! + 1

        for n in neighbors4 {
            let nextPos = Point(x: p.x + n.x, y: p.y + n.y)
            if nextPos.x < 0 || nextPos.y < 0 || nextPos.x > w || nextPos.y > h || nodes[nextPos]!.used > 400 || nextPos == goal {
                continue
            }

            if depth[nextPos] == nil || nextDepth < depth[nextPos]! {
                depth[nextPos] = nextDepth
                pq.append((nextDepth , nextPos))
            }
        }
    }
    fatalError("no possible path")
}

func minMoves(_ nodes: [Point: Node]) -> Int {
    var (w, _) = dim(nodes)
    var goal = Point(x: w, y: 0)
    var hole = findHole(nodes)
    var movesSum = 0

    while goal != Point(x: 0, y: 0) {
        let nextPos = Point(x: goal.x - 1, y: 0)
        movesSum += moves(nodes, goal, hole, nextPos)
        hole = nextPos
        movesSum += moves(nodes, goal, goal, hole)
        (goal, hole) = (hole, goal)
    }

    return movesSum
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Could not open input file")
    }

    var nodes: [Point: Node] = [:]
    let lines = input.components(separatedBy: .newlines).dropFirst(2)

    for line in lines {
        if line.isEmpty { continue }
        let fields = line.split(separator: " ")
        let match = fields[0].replacingOccurrences(of: "/dev/grid/node-x", with: "").replacingOccurrences(of: "-y", with: " ").split(separator: " ")

        
        let p = Point(x: Int(match[0])!, y: Int(match[1])!)
        let used = Int(fields[2].dropLast())!
        let avail = Int(fields[3].dropLast())!
        
        nodes[p] = Node(used: used, avail: avail)
    }

    print(minMoves(nodes))
}

main()
