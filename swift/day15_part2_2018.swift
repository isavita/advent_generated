
import Foundation

struct Point: Hashable, Equatable {
    let x: Int
    let y: Int

    func adjacent() -> [Point] {
        return [
            Point(x: x, y: y - 1),
            Point(x: x - 1, y: y),
            Point(x: x + 1, y: y),
            Point(x: x, y: y + 1)
        ]
    }
}

enum UnitType: Character {
    case elf = "E"
    case goblin = "G"
}

class Unit: Hashable {
    let type: UnitType
    var hp: Int = 200
    var attackPower: Int
    var position: Point

    init(type: UnitType, position: Point, attackPower: Int = 3) {
        self.type = type
        self.position = position
        self.attackPower = attackPower
    }

    static func == (lhs: Unit, rhs: Unit) -> Bool {
        return lhs === rhs
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(ObjectIdentifier(self))
    }
}

func solve(map: [[Character]], elfAttackPower: Int = 3, allowElfDeath: Bool = true) -> (Int, Int, Bool) {
    var grid = map
    var units: [Unit] = []
    var elfCount = 0

    for (y, row) in grid.enumerated() {
        for (x, char) in row.enumerated() {
            if let type = UnitType(rawValue: char) {
                let unit = Unit(type: type, position: Point(x: x, y: y), attackPower: type == .elf ? elfAttackPower : 3)
                units.append(unit)
                if type == .elf {
                    elfCount += 1
                }
            }
        }
    }

    var rounds = 0
    var combatEnded = false

    while !combatEnded {
        units.sort { a, b in
            if a.position.y != b.position.y {
                return a.position.y < b.position.y
            }
            return a.position.x < b.position.x
        }

        for unit in units where unit.hp > 0 {
            if units.filter({ $0.hp > 0 && $0.type != unit.type }).isEmpty {
                combatEnded = true
                break
            }

            // Move
            if !units.contains(where: { $0.hp > 0 && $0.type != unit.type && $0.position.adjacent().contains(unit.position) }) {
                var inRange: Set<Point> = []
                for enemy in units where enemy.hp > 0 && enemy.type != unit.type {
                    for adj in enemy.position.adjacent() {
                        if grid[adj.y][adj.x] == "." {
                            inRange.insert(adj)
                        }
                    }
                }

                if !inRange.isEmpty {
                    let paths = bfs(grid: grid, start: unit.position, targets: inRange)
                    if var bestPath = paths.min(by: { a, b in
                        if a.count != b.count {
                            return a.count < b.count
                        }
                        if a.last!.y != b.last!.y {
                            return a.last!.y < b.last!.y
                        }
                        return a.last!.x < b.last!.x
                    }) {
                        if bestPath.count > 1 {
                            let nextMove = bestPath[1]
                            grid[unit.position.y][unit.position.x] = "."
                            unit.position = nextMove
                            grid[unit.position.y][unit.position.x] = unit.type.rawValue
                        }
                    }
                }
            }

            // Attack
            var targets: [Unit] = []
            for adj in unit.position.adjacent() {
                if let target = units.first(where: { $0.hp > 0 && $0.type != unit.type && $0.position == adj }) {
                    targets.append(target)
                }
            }
            
            targets.sort { a, b in
                if a.hp != b.hp {
                    return a.hp < b.hp
                }
                if a.position.y != b.position.y {
                    return a.position.y < b.position.y
                }
                return a.position.x < b.position.x
            }

            if let target = targets.first {
                target.hp -= unit.attackPower
                if target.hp <= 0 {
                    grid[target.position.y][target.position.x] = "."
                    if !allowElfDeath && target.type == .elf {
                        return (0, 0, false)
                    }
                }
            }
        }

        units = units.filter { $0.hp > 0 }

        if !combatEnded {
            rounds += 1
        }
    }

    let totalHp = units.reduce(0) { $0 + $1.hp }
    let elvesRemaining = units.filter { $0.type == .elf }.count
    let allElvesSurvived = elvesRemaining == elfCount
    
    return (rounds, totalHp, allElvesSurvived)
}

func bfs(grid: [[Character]], start: Point, targets: Set<Point>) -> [[Point]] {
    var queue: [(Point, [Point])] = [(start, [start])]
    var visited: Set<Point> = [start]
    var result: [[Point]] = []

    while !queue.isEmpty {
        let (current, path) = queue.removeFirst()

        if targets.contains(current) {
            result.append(path)
            continue // Keep looking for other paths of the same length
        }

        for adj in current.adjacent() {
            if !visited.contains(adj) && grid[adj.y][adj.x] == "." {
                visited.insert(adj)
                queue.append((adj, path + [adj]))
            }
        }
    }
    
    return result
}


func part1(map: [[Character]]) -> Int {
    let (rounds, totalHp, _) = solve(map: map)
    return rounds * totalHp
}


func part2(map: [[Character]]) -> Int {
    var attackPower = 4
    while true {
        let (rounds, totalHp, allElvesSurvived) = solve(map: map, elfAttackPower: attackPower, allowElfDeath: false)
        if allElvesSurvived {
            return rounds * totalHp
        }
        attackPower += 1
    }
}

func main() {
    do {
        let input = try String(contentsOfFile: "input.txt")
        let map = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n").map { Array($0) }

        print("Part 1: \(part1(map: map))")
        print("Part 2: \(part2(map: map))")
    } catch {
        print("Error reading file: \(error)")
    }
}

main()
