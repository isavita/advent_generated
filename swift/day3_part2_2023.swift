
import Foundation

struct CGPoint: Hashable {
    var x: Int
    var y: Int
    
    static func +(lhs: CGPoint, rhs: CGPoint) -> CGPoint {
        return CGPoint(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
    }
}

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

let Neighbors8 = [
    CGPoint(x: 0, y: 1), CGPoint(x: 0, y: -1), CGPoint(x: 1, y: 0), CGPoint(x: -1, y: 0),
    CGPoint(x: -1, y: -1), CGPoint(x: -1, y: 1), CGPoint(x: 1, y: -1), CGPoint(x: 1, y: 1)
]

struct Part {
    var xmin: Int
    var xmax: Int
    var y: Int
    var n: Int
    
    func valid(grid: [CGPoint: Character]) -> Bool {
        for x in xmin...xmax {
            for n in Neighbors8 {
                if let c = grid[n + CGPoint(x: x, y: y)], c != "." && (c < "0" || c > "9") {
                    return true
                }
            }
        }
        return false
    }
}

let lines = input.components(separatedBy: "\n")
var grid: [CGPoint: Character] = [:]
var parts: [Part] = []
var curr: Part?

for (y, line) in lines.enumerated() {
    if curr != nil {
        parts.append(curr!)
        curr = nil
    }
    for (x, c) in line.enumerated() {
        grid[CGPoint(x: x, y: y)] = c
        if c >= "0" && c <= "9" {
            if curr == nil {
                curr = Part(xmin: x, xmax: x, y: y, n: Int(String(c))!)
            } else {
                curr!.n *= 10
                curr!.n += Int(String(c))!
                curr!.xmax = x
            }
        } else if curr != nil {
            parts.append(curr!)
            curr = nil
        }
    }
}

var partsGrid: [CGPoint: Int] = [:]
for (i, p) in parts.enumerated() {
    for x in p.xmin...p.xmax {
        partsGrid[CGPoint(x: x, y: p.y)] = i
    }
}

var sum = 0
for (p, c) in grid {
    if c == "*" {
        var neighborParts: Set<Int> = []
        for n in Neighbors8 {
            if let i = partsGrid[n + p] {
                neighborParts.insert(i)
            }
        }
        if neighborParts.count == 2 {
            var prod = 1
            for i in neighborParts {
                prod *= parts[i].n
            }
            sum += prod
        }
    }
}
print(sum)
