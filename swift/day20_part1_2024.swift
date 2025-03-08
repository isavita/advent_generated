
import Foundation

func solve() {
    let fileURL = URL(fileURLWithPath: "input.txt")
    guard let input = try? String(contentsOf: fileURL, encoding: .utf8) else {
        return
    }
    let grid = input.components(separatedBy: .newlines).filter { !$0.isEmpty }

    let h = grid.count
    let w = grid[0].count
    var S: (Int, Int) = (0, 0)
    var E: (Int, Int) = (0, 0)

    for r in 0..<h {
        for c in 0..<w {
            let index = grid[r].index(grid[r].startIndex, offsetBy: c)
            if grid[r][index] == "S" {
                S = (r, c)
            } else if grid[r][index] == "E" {
                E = (r, c)
            }
        }
    }

    var walls = [[Bool]](repeating: [Bool](repeating: false, count: w), count: h)
    var trackCells = [(Int, Int)]()

    for r in 0..<h {
        for c in 0..<w {
            let index = grid[r].index(grid[r].startIndex, offsetBy: c)
            if grid[r][index] == "#" {
                walls[r][c] = true
            } else {
                trackCells.append((r, c))
            }
        }
    }

    let dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    func bfs(start: (Int, Int), ignoreWalls: Bool = false) -> [[Int]] {
        var dist = [[Int]](repeating: [Int](repeating: -1, count: w), count: h)
        dist[start.0][start.1] = 0
        var q: [(Int, Int)] = [start]
        var head = 0

        while head < q.count {
            let (r, c) = q[head]
            head += 1

            for (dr, dc) in dirs {
                let nr = r + dr
                let nc = c + dc

                if 0 <= nr && nr < h && 0 <= nc && nc < w {
                    if !ignoreWalls && walls[nr][nc] {
                        continue
                    }
                    if dist[nr][nc] == -1 {
                        dist[nr][nc] = dist[r][c] + 1
                        q.append((nr, nc))
                    }
                }
            }
        }
        return dist
    }

    let distFromS = bfs(start: S)
    let distFromE = bfs(start: E)

    if distFromS[E.0][E.1] == -1 {
        print(0)
        return
    }

    let normalCost = distFromS[E.0][E.1]

    func isTrack(r: Int, c: Int) -> Bool {
        return 0 <= r && r < h && 0 <= c && c < w && !walls[r][c]
    }

    var possibleCheats = 0
    for startPos in trackCells {
        let sd = distFromS[startPos.0][startPos.1]
        if sd == -1 {
            continue
        }

        for (dr1, dc1) in dirs {
            let m1r = startPos.0 + dr1
            let m1c = startPos.1 + dc1
            if !(0 <= m1r && m1r < h && 0 <= m1c && m1c < w) {
                continue
            }
            for (dr2, dc2) in dirs {
                let m2r = m1r + dr2
                let m2c = m1c + dc2
                if !(0 <= m2r && m2r < h && 0 <= m2c && m2c < w) {
                    continue
                }
                if !isTrack(r: m2r, c: m2c) {
                    continue
                }
                let ed = distFromE[m2r][m2c]
                if ed == -1 {
                    continue
                }
                let newCost = sd + 2 + ed
                let saving = normalCost - newCost
                if saving >= 100 {
                    possibleCheats += 1
                }
            }
        }
    }
    print(possibleCheats)
}

solve()
