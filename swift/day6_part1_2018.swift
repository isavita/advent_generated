
import Foundation

struct Point {
    var x: Int
    var y: Int
}

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var points: [Point] = []
var maxX = 0
var maxY = 0

for line in lines {
    let coords = line.components(separatedBy: ", ")
    let x = Int(coords[0])!
    let y = Int(coords[1])!
    if x > maxX {
        maxX = x
    }
    if y > maxY {
        maxY = y
    }
    points.append(Point(x: x, y: y))
}

var grid = [[Int]](repeating: [Int](repeating: 0, count: maxY + 2), count: maxX + 2)
var areas = [Int](repeating: 0, count: points.count)
var infinite = [Bool](repeating: false, count: points.count)

for i in 0..<grid.count {
    for j in 0..<grid[i].count {
        var minDist = maxX + maxY
        for (k, point) in points.enumerated() {
            let dist = abs(point.x - i) + abs(point.y - j)
            if dist < minDist {
                minDist = dist
                grid[i][j] = k
            } else if dist == minDist {
                grid[i][j] = -1
            }
        }
        if grid[i][j] != -1 {
            if i == 0 || j == 0 || i == maxX + 1 || j == maxY + 1 {
                infinite[grid[i][j]] = true
            }
            areas[grid[i][j]] += 1
        }
    }
}

var maxArea = 0
for (i, area) in areas.enumerated() {
    if !infinite[i] && area > maxArea {
        maxArea = area
    }
}
print(maxArea)
