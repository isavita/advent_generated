
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
}

enum Tool: Int, Hashable, CaseIterable {
    case torch = 0
    case climbingGear = 1
    case neither = 2
}

enum RegionType: Int {
    case rocky = 0
    case wet = 1
    case narrow = 2
}

func regionType(erosionLevel: Int) -> RegionType {
    switch erosionLevel % 3 {
    case 0: return .rocky
    case 1: return .wet
    case 2: return .narrow
    default: fatalError("Invalid erosion level")
    }
}

func allowedTools(regionType: RegionType) -> Set<Tool> {
    switch regionType {
    case .rocky:
        return [.torch, .climbingGear]
    case .wet:
        return [.climbingGear, .neither]
    case .narrow:
        return [.torch, .neither]
    }
}

func calculateErosionLevels(depth: Int, target: Point, extra: Int) -> [Point: Int] {
    var erosionLevels: [Point: Int] = [:]
    
    func geologicIndex(for point: Point) -> Int {
        if point == Point(x: 0, y: 0) || point == target {
            return 0
        } else if point.y == 0 {
            return point.x * 16807
        } else if point.x == 0 {
            return point.y * 48271
        } else {
            let p1 = Point(x: point.x - 1, y: point.y)
            let p2 = Point(x: point.x, y: point.y - 1)
            
            
            return erosionLevels[p1]! * erosionLevels[p2]!
        }
    }

    for y in 0...(target.y + extra) {
        for x in 0...(target.x + extra) {
            let point = Point(x: x, y: y)
            let index = geologicIndex(for: point)
            let erosionLevel = (index + depth) % 20183
            erosionLevels[point] = erosionLevel
        }
    }
    return erosionLevels
}


func solvePart1(depth: Int, target: Point) -> Int {
    let erosionLevels = calculateErosionLevels(depth: depth, target: target, extra: 0)
    var totalRisk = 0
    for y in 0...target.y {
        for x in 0...target.x {
            let point = Point(x: x, y: y)
            totalRisk += regionType(erosionLevel: erosionLevels[point]!).rawValue
        }
    }
    return totalRisk
}


func solvePart2(depth: Int, target: Point) -> Int {
    let extra = 100 // Arbitrary extra space.  Adjust as needed. Should be big enough.
    let erosionLevels = calculateErosionLevels(depth: depth, target: target, extra: extra)

    var distances: [Point: [Tool: Int]] = [:]
    var queue: [(Point, Tool, Int)] = []

    let start = Point(x: 0, y: 0)
    distances[start] = [.torch: 0]
    queue.append((start, .torch, 0))

    while !queue.isEmpty {
        queue.sort { $0.2 < $1.2 } // Sort by distance
        let (currentPoint, currentTool, currentDistance) = queue.removeFirst()
        
        if currentPoint == target && currentTool == .torch {
                return currentDistance
        }

        // Change tools
        for nextTool in Tool.allCases {
            if nextTool != currentTool {
                let allowed = allowedTools(regionType: regionType(erosionLevel: erosionLevels[currentPoint]!))
                if allowed.contains(nextTool) {
                    let nextDistance = currentDistance + 7
                    if distances[currentPoint]?[nextTool] == nil || nextDistance < distances[currentPoint]![nextTool]! {
                        distances[currentPoint, default: [:]][nextTool] = nextDistance
                        queue.append((currentPoint, nextTool, nextDistance))
                    }
                }
            }
        }

        // Move to adjacent points
        let neighbors = [
            Point(x: currentPoint.x - 1, y: currentPoint.y),
            Point(x: currentPoint.x + 1, y: currentPoint.y),
            Point(x: currentPoint.x, y: currentPoint.y - 1),
            Point(x: currentPoint.x, y: currentPoint.y + 1)
        ]

        for neighbor in neighbors {
            if neighbor.x >= 0 && neighbor.y >= 0 {
              guard let neighborErosion = erosionLevels[neighbor] else { continue }
                let allowed = allowedTools(regionType: regionType(erosionLevel: neighborErosion))
                if allowed.contains(currentTool) {
                    let nextDistance = currentDistance + 1
                    if distances[neighbor]?[currentTool] == nil || nextDistance < distances[neighbor]![currentTool]! {
                        distances[neighbor, default: [:]][currentTool] = nextDistance
                        queue.append((neighbor, currentTool, nextDistance))
                    }
                }
            }
        }
    }

    return -1 // Should not reach here if a path exists.
}


func main() {
    guard let fileURL = Bundle.main.url(forResource: "input", withExtension: "txt") else {
        fatalError("Input file not found")
    }

    do {
        let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)
        let lines = input.components(separatedBy: .newlines)
        let depth = Int(lines[0].components(separatedBy: ": ")[1])!
        let targetCoords = lines[1].components(separatedBy: ": ")[1].components(separatedBy: ",").map { Int($0)! }
        let target = Point(x: targetCoords[0], y: targetCoords[1])
        
        let part1Result = solvePart1(depth: depth, target: target)
        print("Part 1: \(part1Result)")

        let part2Result = solvePart2(depth: depth, target: target)
        print("Part 2: \(part2Result)")


    } catch {
        fatalError("Error reading input file: \(error)")
    }
}

main()
