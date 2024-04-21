import Foundation

let gridSize = 100
let steps = 100

func countOnNeighbors(_ grid: [[Bool]], x: Int, y: Int) -> Int {
    var on = 0
    for dx in -1...1 {
        for dy in -1...1 {
            if dx == 0 && dy == 0 {
                continue
            }
            let nx = x + dx
            let ny = y + dy
            if nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny] {
                on += 1
            }
        }
    }
    return on
}

func step(_ grid: [[Bool]]) -> [[Bool]] {
    var newGrid = [[Bool]](repeating: [Bool](repeating: false, count: gridSize), count: gridSize)
    
    for x in 0..<gridSize {
        for y in 0..<gridSize {
            let onNeighbors = countOnNeighbors(grid, x: x, y: y)
            if grid[x][y] {
                newGrid[x][y] = onNeighbors == 2 || onNeighbors == 3
            } else {
                newGrid[x][y] = onNeighbors == 3
            }
        }
    }
    
    newGrid[0][0] = true
    newGrid[0][gridSize-1] = true
    newGrid[gridSize-1][0] = true
    newGrid[gridSize-1][gridSize-1] = true
    
    return newGrid
}

func main() {
    let fileURL = URL(fileURLWithPath: "input.txt")
    
    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = content.components(separatedBy: "\n")
        
        var grid = [[Bool]](repeating: [Bool](repeating: false, count: gridSize), count: gridSize)
        
        for (y, line) in lines.enumerated() {
            for (x, char) in line.enumerated() {
                grid[x][y] = char == "#"
            }
        }
        
        grid[0][0] = true
        grid[0][gridSize-1] = true
        grid[gridSize-1][0] = true
        grid[gridSize-1][gridSize-1] = true
        
        for _ in 0..<steps {
            grid = step(grid)
        }
        
        var onCount = 0
        for row in grid {
            for light in row {
                if light {
                    onCount += 1
                }
            }
        }
        
        print(onCount)
    } catch {
        fatalError("Failed to read input.txt")
    }
}

main()