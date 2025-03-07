
import Foundation

class Elf {
    var pos: (Int, Int)
    var moving: Bool = false
    var nextPos: (Int, Int)? = nil

    init(x: Int, y: Int) {
        self.pos = (x, y)
    }

    func aroundAllEmpty(map: [String: Bool], dirs: [(Int, Int)]) -> Bool {
        for (dx, dy) in dirs {
            let adj = (pos.0 + dx, pos.1 + dy)
            if map["\(adj.0),\(adj.1)"] != nil {
                return false
            }
        }
        return true
    }

    func elfInDirection(wannaGo: Int, map: [String: Bool], dirs: [(Int, Int)]) -> Bool {
        for j in -1...1 {
            let dxy = dirs[(wannaGo + j + 8) % 8]
            let adj = (pos.0 + dxy.0, pos.1 + dxy.1)
            if map["\(adj.0),\(adj.1)"] != nil {
                return true
            }
        }
        return false
    }
}

func parse() -> ([Elf], [String: Bool]) {
    var elves: [Elf] = []
    var map: [String: Bool] = [:]

    if let input = try? String(contentsOfFile: "input.txt", encoding: .utf8) {
        let lines = input.components(separatedBy: .newlines).filter { !$0.isEmpty }
        for (row, line) in lines.enumerated() {
            for (col, char) in line.enumerated() {
                if char == "#" {
                    let pos = (row, col)
                    map["\(pos.0),\(pos.1)"] = true
                    elves.append(Elf(x: row, y: col))
                }
            }
        }
    } else {
        print("Error reading input file")
    }

    return (elves, map)
}

func run(elves: [Elf], map: inout [String: Bool], order: [Int], currDir: Int, dirs: [(Int, Int)]) -> Bool {
    var proposes: [String: Int] = [:]

    for e in elves {
        if e.aroundAllEmpty(map: map, dirs: dirs) {
            continue
        }
        for i in 0..<4 {
            let dir = order[(currDir + i) % 4]
            if e.elfInDirection(wannaGo: dir, map: map, dirs: dirs) {
                continue
            }
            let dxy = dirs[dir]
            let dest = (e.pos.0 + dxy.0, e.pos.1 + dxy.1)
            let destKey = "\(dest.0),\(dest.1)"
            proposes[destKey, default: 0] += 1
            e.nextPos = dest
            e.moving = true
            break
        }
    }

    var someoneMoved = false
    for e in elves {
        if !e.moving {
            continue
        }
        guard let nextPos = e.nextPos else { continue }
        let nextPosKey = "\(nextPos.0),\(nextPos.1)"
        if proposes[nextPosKey]! > 1 {
            e.moving = false
            continue
        }
        someoneMoved = true
        map.removeValue(forKey: "\(e.pos.0),\(e.pos.1)")
        map[nextPosKey] = true
        e.pos = nextPos
        e.moving = false
    }

    return someoneMoved
}

func main() {
    let dirs = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]
    let order = [1, 5, 7, 3]  // N, S, W, E (mapping to dirs indices)
    var currDir = 0

    var (elves, map) = parse()

    var i = 0
    while true {
        if !run(elves: elves, map: &map, order: order, currDir: currDir, dirs: dirs) {
            print(i + 1)
            break
        }
        currDir = (currDir + 1) % 4
        i += 1
    }
}

main()
