
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

enum UnitType: String {
    case elf = "E"
    case goblin = "G"
}

class Unit: Hashable, Equatable {
    let id: Int
    var type: UnitType
    var hp: Int = 200
    let attackPower: Int = 3
    var position: Point

    init(id: Int, type: UnitType, position: Point) {
        self.id = id
        self.type = type
        self.position = position
    }

    static func == (lhs: Unit, rhs: Unit) -> Bool {
        return lhs.id == rhs.id
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(id)
    }
}

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Failed to read input file.")
        return
    }

    var grid: [[String]] = input.split(separator: "\n").map { Array($0).map { String($0) } }
    var units: [Unit] = []
    var unitIdCounter = 0

    // Initialize units
    for y in 0..<grid.count {
        for x in 0..<grid[y].count {
            if grid[y][x] == "E" || grid[y][x] == "G" {
                let type = grid[y][x] == "E" ? UnitType.elf : UnitType.goblin
                let unit = Unit(id: unitIdCounter, type: type, position: Point(x: x, y: y))
                units.append(unit)
                unitIdCounter += 1
            }
        }
    }
    
    func findTargets(for unit: Unit, in units: [Unit]) -> [Unit] {
        let enemyType = (unit.type == .elf) ? UnitType.goblin : UnitType.elf
        return units.filter { $0.type == enemyType && $0.hp > 0 }
    }

    func findInRange(targets: [Unit], grid: [[String]]) -> Set<Point> {
        var inRange = Set<Point>()
        for target in targets {
            for adj in target.position.adjacent() {
                if grid[adj.y][adj.x] == "." {
                    inRange.insert(adj)
                }
            }
        }
        return inRange
    }

    func bfs(from start: Point, to targets: Set<Point>, grid: [[String]], units: [Unit]) -> (Point?, Int) {
        var visited = Set<Point>()
        var queue = [(start, 0)]
        var shortestPath: (Point?, Int) = (nil, Int.max)
        var reachableTargets = Set<Point>()
        
        
        visited.insert(start)

        while !queue.isEmpty {
            let (current, distance) = queue.removeFirst()
            
            if targets.contains(current) {
                reachableTargets.insert(current)
                if distance < shortestPath.1{
                    shortestPath = (current, distance)
                } else if distance == shortestPath.1 &&
                            (current.y < shortestPath.0!.y ||
                            (current.y == shortestPath.0!.y && current.x < shortestPath.0!.x)){
                    shortestPath = (current, distance)
                }
                continue
            }
          

            for adj in current.adjacent() {
                if !visited.contains(adj) && grid[adj.y][adj.x] == "." {
                    
                    let occupied = units.contains { $0.position == adj && $0.hp > 0}
                    if !occupied{
                        visited.insert(adj)
                        queue.append((adj, distance + 1))
                    }
                }
            }
        }
        if reachableTargets.isEmpty { return (nil, Int.max)}

        var finalShortestPath: (Point?, Int) = (nil, Int.max)

        queue = [(start, 0)]
        visited = Set<Point>()
        visited.insert(start)
        while !queue.isEmpty {
            let (current, distance) = queue.removeFirst()
            
            if current == shortestPath.0!{
               
                if let firstStep = findFirstStep(start: start, end: current, grid: grid, units: units){
                    
                    if distance < finalShortestPath.1{
                       
                        finalShortestPath = (firstStep, distance)
                    } else if distance == finalShortestPath.1 &&
                                (firstStep.y < finalShortestPath.0!.y ||
                                (firstStep.y == finalShortestPath.0!.y && firstStep.x < finalShortestPath.0!.x)){
                                    finalShortestPath = (firstStep,distance)

                    }
                   
                }
               
                continue
            }
            for adj in current.adjacent() {
                if !visited.contains(adj) && grid[adj.y][adj.x] == "." {
                    
                    let occupied = units.contains { $0.position == adj && $0.hp > 0}
                    if !occupied{
                        visited.insert(adj)
                        queue.append((adj, distance + 1))
                    }
                }
            }
           
        }

        return finalShortestPath
    }

    func findFirstStep(start: Point, end: Point, grid: [[String]], units: [Unit]) -> Point?{
        var visited = Set<Point>()
        var queue = [(start, [start])]
        visited.insert(start)
        var shortestPaths = [[Point]]()

        while !queue.isEmpty {
            let (current, path) = queue.removeFirst()

            if current == end {
                shortestPaths.append(path)
                continue
            }

            for adj in current.adjacent() {
                if !visited.contains(adj) && grid[adj.y][adj.x] == "." {
                    let occupied = units.contains { $0.position == adj && $0.hp > 0 }
                    if !occupied {
                        visited.insert(adj)
                        var newPath = path
                        newPath.append(adj)
                        queue.append((adj, newPath))
                    }
                }
            }
        }

        if shortestPaths.isEmpty { return nil }

        let minLength = shortestPaths.map{$0.count}.min()!
        let realShortestPaths = shortestPaths.filter{$0.count == minLength}

        if realShortestPaths.count == 1{
           return realShortestPaths[0][1]
        } else {
            let firstSteps = realShortestPaths.map{$0[1]}
            var bestStep = firstSteps[0]
            for step in firstSteps{
                if step.y < bestStep.y || (step.y == bestStep.y && step.x < bestStep.x){
                    bestStep = step
                }
            }
            return bestStep
        }
    }
    
    func attack(unit: Unit, targets: inout [Unit], grid: inout [[String]]) {
       
        let adjacentEnemies = targets
                   .filter { unit.position.adjacent().contains($0.position) }
                   .sorted { a, b in
                       if a.hp != b.hp {
                           return a.hp < b.hp
                       } else {
                           return a.position.y < b.position.y || (a.position.y == b.position.y && a.position.x < b.position.x)
                       }
                   }

        if let target = adjacentEnemies.first {
            target.hp -= unit.attackPower
            if target.hp <= 0 {
                grid[target.position.y][target.position.x] = "."
            }
        }
        
    }

    var round = 0
    var combatEnd = false
    while !combatEnd {
    
        units.sort { a, b in
            return a.position.y < b.position.y || (a.position.y == b.position.y && a.position.x < b.position.x)
        }
        
        for (index,unit) in units.enumerated() where unit.hp > 0{
            
            var current_units = units.filter{$0.hp > 0}
            var targets = findTargets(for: unit, in: current_units)
          
            if targets.isEmpty {
                    combatEnd = true
                    break
            }

            if !unit.position.adjacent().contains(where: { adj in targets.contains { $0.position == adj } }) {
            
                let inRangePoints = findInRange(targets: targets, grid: grid)
                
                let (move_point,_) = bfs(from: unit.position, to: inRangePoints, grid: grid, units: current_units)

                if let move_point = move_point{
                        grid[unit.position.y][unit.position.x] = "."
                        unit.position = move_point
                        grid[unit.position.y][unit.position.x] = unit.type.rawValue
                }
            }
            
            current_units = units.filter{$0.hp > 0}
            targets = findTargets(for: unit, in: current_units)
            
            attack(unit: unit, targets: &targets, grid: &grid)
                
            units = units.map{ u in
                if let updated = targets.first(where: {$0.id == u.id}){
                    return updated
                }
                return u
            }
        }
        
        if !combatEnd{
            round += 1
        }
      
    }

    let totalHP = units.filter { $0.hp > 0 }.reduce(0) { $0 + $1.hp }
    print("Combat ends after \(round) full rounds")
    print("Outcome: \(round) * \(totalHP) = \(round * totalHP)")
}
func main() {
    solve()
}

main()
