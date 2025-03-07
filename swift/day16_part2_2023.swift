
import Foundation

func readInput(filePath: String) -> [[Character]] {
    do {
        let fileContent = try String(contentsOfFile: filePath, encoding: .utf8)
        return fileContent.components(separatedBy: .newlines)
            .filter { !$0.isEmpty }
            .map { Array($0) }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

func getEdgeStartPositions(grid: [[Character]]) -> [((Int, Int), (Int, Int))] {
    var starts: [((Int, Int), (Int, Int))] = []
    let height = grid.count
    let width = grid[0].count
    
    for x in 0..<width {
        if grid[0][x] != "#" {
            starts.append(((x, 0), (0, 1)))
        }
        if grid[height - 1][x] != "#" {
            starts.append(((x, height - 1), (0, -1)))
        }
    }
    
    for y in 1..<height - 1 {
        if grid[y][0] != "#" {
            starts.append(((0, y), (1, 0)))
        }
        if grid[y][width - 1] != "#" {
            starts.append(((width - 1, y), (-1, 0)))
        }
    }
    
    return starts
}

func reflect(direction: (Int, Int), mirror: Character) -> (Int, Int) {
    let (dx, dy) = direction
    if mirror == "/" {
        return (-dy, -dx)
    } else if mirror == "\\" {
        return (dy, dx)
    } else {
        return direction
    }
}

func splitBeam(direction: (Int, Int), splitter: Character) -> [(Int, Int)] {
    let (dx, dy) = direction
    if splitter == "|" {
        if dx != 0 {
            return [(0, -1), (0, 1)]
        }
    } else if splitter == "-" {
        if dy != 0 {
            return [(-1, 0), (1, 0)]
        }
    }
    return []
}

func simulateBeam(grid: [[Character]], startPos: (Int, Int), startDir: (Int, Int)) -> Set<String> {
    let height = grid.count
    let width = grid[0].count
    var queue: [((Int, Int), (Int, Int))] = [((startPos.0, startPos.1), startDir)]
    var visited: Set<String> = []
    var energized: Set<String> = []

    while !queue.isEmpty {
        let (pos, direction) = queue.removeFirst()
        let (x, y) = pos
        let state = "\(x),\(y),\(direction.0),\(direction.1)"
        
        if visited.contains(state) {
            continue
        }
        visited.insert(state)
        energized.insert("\(x),\(y)")
        
        let (dx, dy) = direction
        let (nx, ny) = (x + dx, y + dy)
        
        if !(0 <= nx && nx < width && 0 <= ny && ny < height) {
            continue
        }
        
        let cell = grid[ny][nx]
        
        if cell == "." {
            queue.append(((nx, ny), direction))
        } else if cell == "/" || cell == "\\" {
            let newDir = reflect(direction: direction, mirror: cell)
            queue.append(((nx, ny), newDir))
        } else if cell == "|" || cell == "-" {
            let splitDirs = splitBeam(direction: direction, splitter: cell)
            if !splitDirs.isEmpty {
                for newDir in splitDirs {
                    queue.append(((nx, ny), newDir))
                }
            } else {
                queue.append(((nx, ny), direction))
            }
        } else {
            queue.append(((nx, ny), direction))
        }
    }
    
    return energized
}

func partTwo(grid: [[Character]]) -> Int {
    let starts = getEdgeStartPositions(grid: grid)
    var maxEnergized = 0
    
    for (startPos, startDir) in starts {
        let energized = simulateBeam(grid: grid, startPos: startPos, startDir: startDir)
        let count = energized.count
        maxEnergized = max(maxEnergized, count)
    }
    
    return maxEnergized
}

func main() {
    let inputFile = "input.txt"
    let grid = readInput(filePath: inputFile)
    
    let partOneStartPos = (0, 0)
    let partOneStartDir = (1, 0)
    let energizedPartOne = simulateBeam(grid: grid, startPos: partOneStartPos, startDir: partOneStartDir)
    print("Part One Answer: \(energizedPartOne.count)")
    
    let maxEnergizedPartTwo = partTwo(grid: grid)
    print("Part Two Answer: \(maxEnergizedPartTwo)")
}

main()
