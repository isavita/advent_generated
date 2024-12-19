
import Foundation

struct P: Hashable {
    let x: Int
    let y: Int

    func neighbours() -> [P] {
        return [
            P(x: x, y: y + 1),
            P(x: x + 1, y: y),
            P(x: x, y: y - 1),
            P(x: x - 1, y: y)
        ]
    }
}

func isLetter(_ c: Character) -> Bool {
    return c >= "A" && c <= "Z"
}

func parse() -> (grid: [P: Character], aa: P, zz: P, teleport: [P: P]) {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try! String(contentsOf: fileURL)
    let lines = content.split(separator: "\n")

    var grid: [P: Character] = [:]
    var xMax = 0
    var yMax = 0

    for (i, line) in lines.enumerated() {
        xMax = max(xMax, i)
        yMax = max(yMax, line.count)
        for (j, char) in line.enumerated() {
            grid[P(x: i, y: j)] = char
        }
    }

    var aa = P(x: 0, y: 0)
    var zz = P(x: 0, y: 0)
    var teleport: [P: P] = [:]
    var cache: [String: P] = [:]

    for i in 0...xMax {
        for j in 0...yMax {
            let p = P(x: i, y: j)
            let c = grid[p]!

            if !isLetter(c) {
                continue
            }

            if let (portalName, portalPoint) = extractPortal(grid: grid, p: p) {
                if portalName == "AA" {
                    aa = portalPoint
                } else if portalName == "ZZ" {
                    zz = portalPoint
                } else if let target = cache[portalName] {
                    teleport[portalPoint] = target
                    teleport[target] = portalPoint
                } else {
                    cache[portalName] = portalPoint
                }
            }
        }
    }

    return (grid, aa, zz, teleport)
}

func extractPortal(grid: [P: Character], p: P) -> (String, P)? {
    let c1 = grid[p]!

    if let c2 = grid[P(x: p.x + 1, y: p.y)], isLetter(c2) {
        let portalName = String(c1) + String(c2)
        if let portalPoint = [P(x: p.x + 2, y: p.y), P(x: p.x - 1, y: p.y)].first(where: { grid[$0] == "." }) {
            return (portalName, portalPoint)
        }
    }

    if let c2 = grid[P(x: p.x, y: p.y + 1)], isLetter(c2) {
        let portalName = String(c1) + String(c2)
        if let portalPoint = [P(x: p.x, y: p.y + 2), P(x: p.x, y: p.y - 1)].first(where: { grid[$0] == "." }) {
            return (portalName, portalPoint)
        }
    }

    return nil
}

func bfs(grid: [P: Character], aa: P, zz: P, teleport: [P: P]) -> Int {
    var discovered: Set<P> = []
    var toDo: [P] = []

    discovered.insert(aa)
    toDo.append(aa)

    var depth = 0

    while !toDo.isEmpty {
        var nextToDo: [P] = []
        for curr in toDo {
            if curr == zz {
                return depth
            }

            for n in curr.neighbours() {
                guard let dest = grid[n] else { continue }

                if dest == "#" {
                    continue
                }

                if dest == "." {
                    if !discovered.contains(n) {
                        discovered.insert(n)
                        nextToDo.append(n)
                    }
                } else if isLetter(dest) {
                    if let next = teleport[curr], !discovered.contains(next) {
                        discovered.insert(next)
                        nextToDo.append(next)
                    }
                }
            }
        }
        toDo = nextToDo
        depth += 1
    }

    return -1
}

let (grid, aa, zz, teleport) = parse()
print(bfs(grid: grid, aa: aa, zz: zz, teleport: teleport))
