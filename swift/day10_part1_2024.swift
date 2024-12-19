
import Foundation

struct Pos: Hashable {
    let r: Int
    let c: Int
}

func solve() -> Int {
    guard let data = try? String(contentsOfFile: "input.txt") else { return 0 }
    let lines = data.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
    let nr = lines.count
    let nc = lines[0].count
    var grid = [[Int]]()
    for line in lines {
        grid.append(line.map { Int(String($0))! })
    }

    let dirs = [Pos(r: 1, c: 0), Pos(r: -1, c: 0), Pos(r: 0, c: 1), Pos(r: 0, c: -1)]
    var trailheads = [Pos]()
    for r in 0..<nr {
        for c in 0..<nc {
            if grid[r][c] == 0 {
                trailheads.append(Pos(r: r, c: c))
            }
        }
    }

    var sumScores = 0
    for th in trailheads {
        var reached = Set<Pos>()
        var front = [(p: th, h: 0)]
        var visited = Set<[Int]>()

        while !front.isEmpty {
            let cur = front.removeLast()
            if cur.h == 9 {
                reached.insert(cur.p)
                continue
            }
            for d in dirs {
                let nr2 = cur.p.r + d.r
                let nc2 = cur.p.c + d.c
                if nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc { continue }
                if grid[nr2][nc2] == cur.h + 1 {
                    let key = [nr2, nc2, cur.h + 1]
                    if !visited.contains(key) {
                        visited.insert(key)
                        front.append((p: Pos(r: nr2, c: nc2), h: cur.h + 1))
                    }
                }
            }
        }
        sumScores += reached.count
    }
    return sumScores
}

print(solve())
