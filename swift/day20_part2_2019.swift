
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

struct Map {
    let xMax: Int
    let yMax: Int
    let grid: [P: Character]
    let aa: P
    let zz: P
    let teleport: [P: P]
    let portalName: [P: String]
    let isOuter: [P: Bool]
}

func parse() -> Map {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try! String(contentsOf: fileURL)
    let lines = content.split(separator: "\n")

    var grid: [P: Character] = [:]
    var xMax = 0
    var yMax = 0

    for (i, line) in lines.enumerated() {
        xMax = max(xMax, i + 1)
        yMax = max(yMax, line.count)
        for (j, char) in line.enumerated() {
            grid[P(x: i, y: j)] = char
        }
    }

    var aa = P(x: 0, y: 0)
    var zz = P(x: 0, y: 0)
    var isOuter: [P: Bool] = [:]
    var portalName: [P: String] = [:]
    var teleport: [P: P] = [:]
    var cache: [String: P] = [:]

    for i in 0..<xMax {
        for j in 0..<yMax {
            let p = P(x: i, y: j)
            guard let c = grid[p], c.isUppercase else { continue }

            if let (name, point, ok) = extractPortal(grid: grid, p: p), ok {
                portalName[point] = name

                if name == "AA" {
                    aa = point
                    isOuter[point] = true
                    continue
                }

                if name == "ZZ" {
                    zz = point
                    isOuter[point] = true
                    continue
                }

                if let target = cache[name] {
                    teleport[point] = target
                    teleport[target] = point
                } else {
                    cache[name] = point
                }

                isOuter[point] = (j == 0 || i == 0 || i == xMax - 2 || j == yMax - 2)
            }
        }
    }

    return Map(
        xMax: xMax,
        yMax: yMax,
        grid: grid,
        aa: aa,
        zz: zz,
        teleport: teleport,
        portalName: portalName,
        isOuter: isOuter
    )
}

func extractPortal(grid: [P: Character], p: P) -> (String, P, Bool)? {
    guard let c1 = grid[p], c1.isUppercase else { return nil }

    if let c2 = grid[P(x: p.x + 1, y: p.y)], c2.isUppercase {
        let portalName = String(c1) + String(c2)
        if let portalPoint = [P(x: p.x + 2, y: p.y), P(x: p.x - 1, y: p.y)].first(where: { grid[$0] == "." }) {
            return (portalName, portalPoint, true)
        }
    }

    if let c2 = grid[P(x: p.x, y: p.y + 1)], c2.isUppercase {
        let portalName = String(c1) + String(c2)
        if let portalPoint = [P(x: p.x, y: p.y + 2), P(x: p.x, y: p.y - 1)].first(where: { grid[$0] == "." }) {
            return (portalName, portalPoint, true)
        }
    }

    return nil
}

func bfsNested(m: Map) -> Int {
    struct Status: Hashable {
        let p: P
        let depth: Int
    }

    var discovered: Set<Status> = []
    var toDo: [Status] = []

    let root = Status(p: m.aa, depth: 0)
    discovered.insert(root)
    toDo.append(root)

    var steps = 0

    while !toDo.isEmpty {
        var nextToDo: [Status] = []
        for curr in toDo {
            for n in curr.p.neighbours() {
                guard let dest = m.grid[n] else { continue }

                switch dest {
                case "#":
                    continue
                case ".":
                    let target = Status(p: n, depth: curr.depth)
                    if !discovered.contains(target) {
                        discovered.insert(target)
                        nextToDo.append(target)
                    }
                default:
                    if dest.isUppercase {
                        var target: Status
                        if let isOuter = m.isOuter[curr.p], !isOuter {
                            target = Status(p: m.teleport[curr.p]!, depth: curr.depth + 1)
                        } else {
                            let portalName = m.portalName[curr.p]!
                            if curr.depth == 0 {
                                if portalName == "ZZ" {
                                    return steps
                                }
                                continue
                            }
                            if portalName == "AA" || portalName == "ZZ" {
                                continue
                            }
                            target = Status(p: m.teleport[curr.p]!, depth: curr.depth - 1)
                        }

                        if !discovered.contains(target) {
                            discovered.insert(target)
                            nextToDo.append(target)
                        }
                    }
                }
            }
        }
        toDo = nextToDo
        steps += 1
    }

    return -1
}

let m = parse()
print(bfsNested(m: m))
