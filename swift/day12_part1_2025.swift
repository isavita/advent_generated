
import Foundation

struct Point: Equatable, Hashable {
    var r: Int
    var c: Int
}

struct Piece {
    var cells: [Point]
    var count: Int = 0
}

func normalize(_ p: [Point]) -> [Point] {
    guard let minR = p.map({ $0.r }).min(),
          let minC = p.map({ $0.c }).min() else { return [] }
    let shifted = p.map { Point(r: $0.r - minR, c: $0.c - minC) }
    return shifted.sorted { $0.r == $1.r ? $0.c < $1.c : $0.r < $1.r }
}

func rotate(_ p: [Point]) -> [Point] {
    p.map { Point(r: $0.c, c: -$0.r) }
}

func flip(_ p: [Point]) -> [Point] {
    p.map { Point(r: $0.r, c: -$0.c) }
}

func equal(_ a: [Point], _ b: [Point]) -> Bool { a == b }

func generateVariations(_ base: [Point]) -> [[Point]] {
    var uniq = Set<[Point]>()
    var cur = base
    for _ in 0..<4 {
        let n = normalize(cur)
        uniq.insert(n)
        let f = normalize(flip(cur))
        uniq.insert(f)
        cur = rotate(cur)
    }
    return Array(uniq)
}

func canPlace(_ rows: Int, _ cols: Int, _ grid: [UInt8], _ piece: [Point], _ r: Int, _ c: Int) -> Bool {
    for p in piece {
        let nr = r + p.r, nc = c + p.c
        if nr < 0 || nr >= rows || nc < 0 || nc >= cols { return false }
        if grid[nr * cols + nc] != 0 { return false }
    }
    return true
}

func place(_ cols: Int, _ grid: inout [UInt8], _ piece: [Point], _ r: Int, _ c: Int, _ val: UInt8) {
    for p in piece { grid[(r + p.r) * cols + (c + p.c)] = val }
}

func checkIslands(_ rows: Int, _ cols: Int, _ grid: [UInt8],
                  _ counts: [Int], _ slackIdx: Int,
                  _ shapes: [Piece]) -> Bool {
    var minReal = Int.max
    var hasReal = false
    for i in 0..<shapes.count where i != slackIdx && counts[i] > 0 {
        minReal = min(minReal, shapes[i].cells.count)
        hasReal = true
    }
    if !hasReal { return true }
    var slack = counts[slackIdx]
    var visited = [Bool](repeating: false, count: rows * cols)
    var q = [Int]()
    for i in 0..<(rows * cols) where grid[i] == 0 && !visited[i] {
        q.removeAll(keepingCapacity: true)
        q.append(i)
        visited[i] = true
        var qs = 0, size = 0
        while qs < q.count {
            let cur = q[qs]; qs += 1; size += 1
            let r = cur / cols, c = cur % cols
            let dirs = [(-1,0),(1,0),(0,-1),(0,1)]
            for d in dirs {
                let nr = r + d.0, nc = c + d.1
                if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
                    let ni = nr * cols + nc
                    if grid[ni] == 0 && !visited[ni] {
                        visited[ni] = true
                        q.append(ni)
                    }
                }
            }
        }
        if size < minReal {
            if slack >= size { slack -= size } else { return false }
        }
    }
    return true
}

func solve(_ rows: Int, _ cols: Int, _ grid: inout [UInt8],
           _ counts: inout [Int], _ ids: [Int],
           _ variations: [[ [Point] ]], _ slackIdx: Int,
           _ shapes: [Piece]) -> Bool {
    guard let empty = grid.firstIndex(of: 0) else { return true }
    let r = empty / cols, c = empty % cols
    if !checkIslands(rows, cols, grid, counts, slackIdx, shapes) { return false }
    for id in ids where counts[id] > 0 {
        counts[id] -= 1
        for varPiece in variations[id] {
            if canPlace(rows, cols, grid, varPiece, r, c) {
                place(cols, &grid, varPiece, r, c, 1)
                if solve(rows, cols, &grid, &counts, ids, variations, slackIdx, shapes) { return true }
                place(cols, &grid, varPiece, r, c, 0)
            }
        }
        counts[id] += 1
    }
    return false
}

/* ---------- main ---------- */

let data = try! String(contentsOfFile: "input.txt", encoding: .utf8)
let rawLines = data.components(separatedBy: .newlines)

func trim(_ s: String) -> String {
    var t = s
    while let first = t.unicodeScalars.first, CharacterSet.whitespacesAndNewlines.contains(first) {
        t.removeFirst()
    }
    while let last = t.unicodeScalars.last, CharacterSet.whitespacesAndNewlines.contains(last) {
        t.removeLast()
    }
    return t
}

/* find max id */
var maxId = -1
for l in rawLines {
    let s = trim(l)
    if s.hasSuffix(":") {
        let num = Int(s.dropLast()) ?? -1
        maxId = max(maxId, num)
    }
}
if maxId < 0 { maxId = -1 }
let slackIdx = maxId + 1
let arrSize = slackIdx + 1

var shapes = [Piece](repeating: Piece(cells: []), count: arrSize)

var parsingShapes = true
var currentID = -1
var shapeLines = [String]()

var regionLines = [String]()

for raw in rawLines {
    let s = trim(raw)
    if s.isEmpty { continue }
    if s.contains("x") && s.contains(":") { parsingShapes = false }
    if parsingShapes {
        if s.hasSuffix(":") {
            if currentID != -1 && !shapeLines.isEmpty {
                var pts = [Point]()
                for (r, line) in shapeLines.enumerated() {
                    for (c, ch) in line.enumerated() where ch == "#" {
                        pts.append(Point(r: r, c: c))
                    }
                }
                shapes[currentID] = Piece(cells: normalize(pts))
                shapeLines.removeAll()
            }
            currentID = Int(s.dropLast()) ?? -1
        } else {
            shapeLines.append(s)
        }
    } else {
        regionLines.append(s)
    }
}
if currentID != -1 && !shapeLines.isEmpty {
    var pts = [Point]()
    for (r, line) in shapeLines.enumerated() {
        for (c, ch) in line.enumerated() where ch == "#" {
            pts.append(Point(r: r, c: c))
        }
    }
    shapes[currentID] = Piece(cells: normalize(pts))
}

/* slack piece */
shapes[slackIdx] = Piece(cells: [Point(r: 0, c: 0)])

/* variations */
var variations = [[ [Point] ]](repeating: [], count: arrSize)
for i in 0..<arrSize where !shapes[i].cells.isEmpty {
    variations[i] = generateVariations(shapes[i].cells)
}

/* solve each region */
var solved = 0
for line in regionLines {
    guard let colonPos = line.firstIndex(of: ":") else { continue }
    let dims = line[..<colonPos]
    var countsStr = line[line.index(after: colonPos)...]
    let dimParts = dims.split(separator: "x")
    guard dimParts.count == 2,
          let w = Int(dimParts[0]),
          let h = Int(dimParts[1]) else { continue }
    var pieceCounts = [Int](repeating: 0, count: arrSize)
    var total = 0
    let tokens = countsStr.split(whereSeparator: { $0 == " " || $0 == "\t" })
    for (idx, tok) in tokens.enumerated() where idx < arrSize - 1 {
        let c = Int(tok) ?? 0
        if c > 0 {
            pieceCounts[idx] = c
            total += c * shapes[idx].cells.count
        }
    }
    if total > w * h { continue }
    let slack = w * h - total
    if slack > 0 { pieceCounts[slackIdx] = slack }

    var ids = [Int]()
    for i in 0..<arrSize where pieceCounts[i] > 0 { ids.append(i) }
    ids.sort { shapes[$0].cells.count > shapes[$1].cells.count }

    var grid = [UInt8](repeating: 0, count: w * h)
    var mutableCounts = pieceCounts
    if solve(h, w, &grid, &mutableCounts, ids, variations, slackIdx, shapes) {
        solved += 1
    }
}

print("Number of regions that fit all presents: \(solved)")
