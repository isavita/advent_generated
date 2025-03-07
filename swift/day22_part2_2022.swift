
import Foundation

struct P: Hashable {
    let x: Int
    let y: Int
}

enum Dir: Int {
    case N = 0
    case E = 1
    case S = 2
    case W = 3

    func rotate(direction: Character) -> Dir {
        switch direction {
        case "R":
            return Dir(rawValue: (self.rawValue + 1) % 4)!
        case "L":
            return Dir(rawValue: (self.rawValue - 1 + 4) % 4)!
        default:
            return self
        }
    }

    func points() -> Int {
        return (self.rawValue + 3) % 4
    }
}

struct Movement {
    let steps: Int
    let rotate: Character?

    init(steps: Int) {
        self.steps = steps
        self.rotate = nil
    }

    init(rotate: Character) {
        self.rotate = rotate
        self.steps = 0
    }
}

struct Human {
    var curr: P
    var facing: Dir

    mutating func walk(mapData: [P: Bool], dirs: [P], size: Int) -> Void {
        let dirDelta = dirs[facing.rawValue]
        let nextPos = P(x: curr.x + dirDelta.x, y: curr.y + dirDelta.y)

        if let isWall = mapData[nextPos] {
            if isWall {
                return
            } else {
                curr = nextPos
                return
            }
        } else {
            let (newPos, newFacing) = crossBorder(n: nextPos, dir: facing, size: size)
            if let isWall = mapData[newPos] {
                if isWall {
                    return
                }
            }
            curr = newPos
            facing = newFacing
            return
        }
    }
}

func crossBorder(n: P, dir: Dir, size: Int) -> (P, Dir) {
    let x = n.x
    let y = n.y
    let S = size

    if x == -1 && y < 2 * S {
        return (P(x: y + 2 * S, y: x + 1), Dir.E)
    } else if x == -1 && y >= 2 * S {
        return (P(x: x + 4 * S, y: y - 2 * S), Dir.N)
    } else if x == S && dir == Dir.S {
        return (P(x: y - S, y: x + S - 1), Dir.W)
    } else if x == 2 * S - 1 && dir == Dir.N {
        return (P(x: y + S, y: x - S + 1), Dir.E)
    } else if x == 3 * S && dir == Dir.S {
        return (P(x: y + 2 * S, y: x - 2 * S - 1), Dir.W)
    } else if x == 4 * S {
        return (P(x: x - 4 * S, y: y + 2 * S), Dir.S)
    } else if y == -1 && x < 3 * S {
        return (P(x: 3 * S - 1 - x, y: y + S + 1), Dir.E)
    } else if y == -1 && x >= 3 * S {
        return (P(x: y + 1, y: x - 2 * S), Dir.S)
    } else if y == S - 1 && x < S {
        return (P(x: 3 * S - 1 - x, y: y - S + 1), Dir.E)
    } else if y == S - 1 && x >= S && dir == Dir.W {
        return (P(x: y + S + 1, y: x - S), Dir.S)
    } else if y == S && dir == Dir.E {
        return (P(x: y + 2 * S - 1, y: x - 2 * S), Dir.N)
    } else if y == 2 * S && x < 2 * S && dir == Dir.E {
        return (P(x: y - S - 1, y: x + S), Dir.N)
    } else if y == 2 * S && x >= 2 * S {
        return (P(x: 3 * S - 1 - x, y: y + S - 1), Dir.W)
    } else if y == 3 * S {
        return (P(x: 3 * S - 1 - x, y: y - S - 1), Dir.W)
    } else {
        fatalError("Not a border crossing")
    }
}

func parsePath(path: String) -> [Movement] {
    var movements: [Movement] = []
    var acc = 0

    for char in path {
        if char == "R" || char == "L" {
            if acc != 0 {
                movements.append(Movement(steps: acc))
                acc = 0
            }
            movements.append(Movement(rotate: char))
        } else if char.isNumber {
            acc = acc * 10 + Int(String(char))!
        }
    }

    if acc != 0 {
        movements.append(Movement(steps: acc))
    }
    return movements
}

func parseInput(filename: String) -> ([P: Bool], Int, [Movement]) {
    var mapData: [P: Bool] = [:]
    var size = 0
    var movements: [Movement] = []

    do {
        let fileContent = try String(contentsOfFile: filename, encoding: .utf8)
        var lines = fileContent.components(separatedBy: .newlines)

        var r = 0
        while r < lines.count {
            let line = lines[r]
            if line == "" {
                break
            }
            if r == 0 {
                size = line.count / 3
            }
            for (c, char) in line.enumerated() {
                if char == " " {
                    continue
                } else if char == "#" {
                    mapData[P(x: r, y: c)] = true
                } else if char == "." {
                    mapData[P(x: r, y: c)] = false
                }
            }
            r += 1
        }

        let movementLine = lines[r + 1].trimmingCharacters(in: .whitespacesAndNewlines)
        movements = parsePath(path: movementLine)

        return (mapData, size, movements)

    } catch {
        fatalError("Error reading file: \(error)")
    }
}

func main() {
    let (mapData, size, movements) = parseInput(filename: "input.txt")
    let dirs = [
        P(x: -1, y: 0),
        P(x: 0, y: 1),
        P(x: 1, y: 0),
        P(x: 0, y: -1)
    ]

    var human = Human(curr: P(x: 0, y: size), facing: .E)

    for mov in movements {
        if let rotate = mov.rotate {
            human.facing = human.facing.rotate(direction: rotate)
        } else {
            for _ in 0..<mov.steps {
                let originalPos = human.curr
                let originalFacing = human.facing

                human.walk(mapData: mapData, dirs: dirs, size: size)
                if human.curr == originalPos && human.facing == originalFacing {
                    break
                }
            }
        }
    }

    let finalValue = 1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points()
    print(finalValue)
}

main()
