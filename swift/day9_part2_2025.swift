
import Foundation

struct Point { var x: Int; var y: Int }

let content = try String(contentsOfFile: "input.txt")
let lines = content.split(separator: "\n")
var points = [Point]()
var uniqueXMap = Set<Int>()
var uniqueYMap = Set<Int>()

for raw in lines {
    let line = raw.trimmingCharacters(in: .whitespaces)
    if line.isEmpty { continue }
    let parts = line.split(separator: ",")
    if parts.count != 2 { continue }
    if let x = Int(parts[0]), let y = Int(parts[1]) {
        points.append(Point(x: x, y: y))
        uniqueXMap.insert(x)
        uniqueYMap.insert(y)
    }
}

guard !points.isEmpty else { exit(0) }

let uniqueX = uniqueXMap.sorted()
let uniqueY = uniqueYMap.sorted()
var xMap = [Int: Int]()
var yMap = [Int: Int]()
for (i, v) in uniqueX.enumerated() { xMap[v] = i }
for (i, v) in uniqueY.enumerated() { yMap[v] = i }

let gridW = 2 * uniqueX.count + 1
let gridH = 2 * uniqueY.count + 1
var colWidths = [Int64](repeating: 0, count: gridW)
var rowHeights = [Int64](repeating: 0, count: gridH)

colWidths[0] = 1
for i in 0..<uniqueX.count {
    colWidths[2*i+1] = 1
    if i < uniqueX.count - 1 {
        let gap = max(0, uniqueX[i+1] - uniqueX[i] - 1)
        colWidths[2*i+2] = Int64(gap)
    } else {
        colWidths[2*i+2] = 1
    }
}
rowHeights[0] = 1
for i in 0..<uniqueY.count {
    rowHeights[2*i+1] = 1
    if i < uniqueY.count - 1 {
        let gap = max(0, uniqueY[i+1] - uniqueY[i] - 1)
        rowHeights[2*i+2] = Int64(gap)
    } else {
        rowHeights[2*i+2] = 1
    }
}

var grid = [[Int8]](repeating: [Int8](repeating: 0, count: gridW), count: gridH)
func toGrid(_ p: Point) -> (Int, Int) {
    let gx = 2 * (xMap[p.x] ?? 0) + 1
    let gy = 2 * (yMap[p.y] ?? 0) + 1
    return (gx, gy)
}
let n = points.count
for i in 0..<n {
    let p1 = points[i]
    let p2 = points[(i+1)%n]
    let (gx1, gy1) = toGrid(p1)
    let (gx2, gy2) = toGrid(p2)
    if gx1 == gx2 {
        var start = gy1, end = gy2
        if start > end { swap(&start, &end) }
        for y in start...end where rowHeights[y] > 0 {
            grid[y][gx1] = 1
        }
    } else {
        var start = gx1, end = gx2
        if start > end { swap(&start, &end) }
        for x in start...end where colWidths[x] > 0 {
            grid[gy1][x] = 1
        }
    }
}

var queue = [Point]()
queue.append(Point(x: 0, y: 0))
grid[0][0] = 2
var head = 0
let dirs = [Point(x: 0, y: 1), Point(x: 0, y: -1), Point(x: 1, y: 0), Point(x: -1, y: 0)]

while head < queue.count {
    let cur = queue[head]; head += 1
    for d in dirs {
        let nx = cur.x + d.x
        let ny = cur.y + d.y
        if nx >= 0 && nx < gridW && ny >= 0 && ny < gridH && grid[ny][nx] == 0 {
            grid[ny][nx] = 2
            queue.append(Point(x: nx, y: ny))
        }
    }
}

var pref = [[Int64]](repeating: [Int64](repeating: 0, count: gridW), count: gridH)
for y in 0..<gridH {
    for x in 0..<gridW {
        var val: Int64 = 0
        if grid[y][x] != 2 { val = colWidths[x] * rowHeights[y] }
        let left = x > 0 ? pref[y][x-1] : 0
        let up = y > 0 ? pref[y-1][x] : 0
        let diag = (x > 0 && y > 0) ? pref[y-1][x-1] : 0
        pref[y][x] = val + left + up - diag
    }
}
func getSum(_ x1: Int, _ y1: Int, _ x2: Int, _ y2: Int) -> Int64 {
    var a = x1, b = x2
    if a > b { swap(&a, &b) }
    var c = y1, d = y2
    if c > d { swap(&c, &d) }
    var res = pref[d][b]
    if a > 0 { res -= pref[d][a-1] }
    if c > 0 { res -= pref[c-1][b] }
    if a > 0 && c > 0 { res += pref[c-1][a-1] }
    return res
}

var maxArea: Int64 = 0
for i in 0..<points.count {
    for j in i..<points.count {
        let p1 = points[i], p2 = points[j]
        let realW = Int64(abs(p1.x - p2.x) + 1)
        let realH = Int64(abs(p1.y - p2.y) + 1)
        let area = realW * realH
        if area <= maxArea { continue }
        let (gx1, gy1) = toGrid(p1)
        let (gx2, gy2) = toGrid(p2)
        if getSum(gx1, gy1, gx2, gy2) == area {
            maxArea = area
        }
    }
}
print("Largest valid area: \(maxArea)")
