
import Foundation

struct Point: Hashable {
    let x, y: Int
}

let fileURL = URL(fileURLWithPath: "input.txt")
let content = try String(contentsOf: fileURL)
let grid = content.split(separator: "\n").map { String($0) }

let h = grid.count
let w = grid[0].count
var S = Point(x: 0, y: 0)
var E = Point(x: 0, y: 0)
var walls = [[Bool]](repeating: [Bool](repeating: false, count: w), count: h)
var trackCells = [Point]()

for i in 0..<h {
    for j in 0..<w {
        let ch = grid[i][grid[i].index(grid[i].startIndex, offsetBy: j)]
        if ch == "S" {
            S = Point(x: i, y: j)
        } else if ch == "E" {
            E = Point(x: i, y: j)
        }
        if ch == "#" {
            walls[i][j] = true
        } else {
            trackCells.append(Point(x: i, y: j))
        }
    }
}

let dirs = [Point(x: 1, y: 0), Point(x: -1, y: 0), Point(x: 0, y: 1), Point(x: 0, y: -1)]

func isTrack(x: Int, y: Int) -> Bool {
    return x >= 0 && x < h && y >= 0 && y < w && !walls[x][y]
}

func normalDistFrom(start: Point) -> [[Int]] {
    var dist = [[Int]](repeating: [Int](repeating: -1, count: w), count: h)
    dist[start.x][start.y] = 0
    var q = [start]
    var head = 0
    while head < q.count {
        let cur = q[head]
        head += 1
        for d in dirs {
            let nx = cur.x + d.x
            let ny = cur.y + d.y
            if nx < 0 || nx >= h || ny < 0 || ny >= w { continue }
            if walls[nx][ny] { continue }
            if dist[nx][ny] < 0 {
                dist[nx][ny] = dist[cur.x][cur.y] + 1
                q.append(Point(x: nx, y: ny))
            }
        }
    }
    return dist
}

let distFromS = normalDistFrom(start: S)
let distFromE = normalDistFrom(start: E)

if distFromS[E.x][E.y] < 0 {
    print(0)
} else {
    let normalCost = distFromS[E.x][E.y]
    var cheats = [Point: [Point: Int]]()

    for startPos in trackCells {
        let sd = distFromS[startPos.x][startPos.y]
        if sd < 0 { continue }

        var distC = [[Int]](repeating: [Int](repeating: -1, count: w), count: h)
        distC[startPos.x][startPos.y] = 0
        var q = [startPos]
        var head = 0

        while head < q.count {
            let cur = q[head]
            head += 1
            let steps = distC[cur.x][cur.y]
            if steps == 20 { continue }
            for d in dirs {
                let nx = cur.x + d.x
                let ny = cur.y + d.y
                if nx < 0 || nx >= h || ny < 0 || ny >= w { continue }
                if distC[nx][ny] < 0 {
                    distC[nx][ny] = steps + 1
                    q.append(Point(x: nx, y: ny))
                }
            }
        }
        
        for x in 0..<h {
            for y in 0..<w {
                let s = distC[x][y]
                if s > 0 && s <= 20 && isTrack(x: x, y: y) {
                    let ed = distFromE[x][y]
                    if ed < 0 { continue }
                    let cost = sd + s + ed
                    if cost < normalCost {
                        if cheats[startPos] == nil {
                            cheats[startPos] = [:]
                        }
                        if let old = cheats[startPos]?[Point(x:x,y:y)], cost >= old {
                            continue
                        }
                        cheats[startPos]?[Point(x:x,y:y)] = cost
                    }
                }
            }
        }
    }

    var count = 0
    for (_, endPoints) in cheats {
        for (_, cost) in endPoints {
            let saving = normalCost - cost
            if saving >= 100 {
                count += 1
            }
        }
    }
    print(count)
}
