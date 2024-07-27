
import Foundation

struct Point: Hashable {
    var x: Int
    var y: Int
}

func readInput() -> [String] {
    let url = URL(fileURLWithPath: "input.txt")
    return try! String(contentsOf: url).split(separator: "\n").map(String.init)
}

func parsePoints(from line: String) -> [Point] {
    return line.split(separator: " -> ").map { part in
        let coords = part.split(separator: ",").map { Int($0)! }
        return Point(x: coords[0], y: coords[1])
    }
}

func fill(grid: inout Set<Point>) -> Int {
    let floor = grid.map { $0.y }.max()! + 1
    var sands = 0, firstFloorTouch = 0
    var full = false
    
    while !full {
        var sand = Point(x: 500, y: 0)
        var settled = false
        
        while !settled {
            if sand.y == floor {
                if firstFloorTouch == 0 {
                    firstFloorTouch = sands
                }
                grid.insert(sand)
                settled = true
                break
            }
            let nextPositions = [Point(x: sand.x, y: sand.y + 1), Point(x: sand.x - 1, y: sand.y + 1), Point(x: sand.x + 1, y: sand.y + 1)]
            if let next = nextPositions.first(where: { !grid.contains($0) }) {
                sand = next
            } else {
                grid.insert(sand)
                settled = true
            }
        }
        sands += 1
        full = grid.contains(Point(x: 500, y: 0))
    }
    return firstFloorTouch
}

func main() {
    var grid = Set<Point>()
    for line in readInput() {
        let points = parsePoints(from: line)
        for i in 0..<points.count - 1 {
            let start = points[i]
            let end = points[i + 1]
            if start.x == end.x {
                for y in min(start.y, end.y)...max(start.y, end.y) {
                    grid.insert(Point(x: start.x, y: y))
                }
            } else {
                for x in min(start.x, end.x)...max(start.x, end.x) {
                    grid.insert(Point(x: x, y: start.y))
                }
            }
        }
    }
    print(fill(grid: &grid))
}

main()
