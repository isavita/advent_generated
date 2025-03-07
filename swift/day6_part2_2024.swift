
import Foundation

struct State: Hashable {
    let x: Int
    let y: Int
    let dirIdx: Int
}

func loops(grid: inout [[Character]], startX: Int, startY: Int, startDir: Int) -> Bool {
    let h = grid.count
    let w = grid[0].count
    let dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)] // up, right, down, left
    
    var x = startX
    var y = startY
    var dirIdx = startDir
    var seen: Set<State> = []
    
    for _ in 0..<2000000 {
        let state = State(x: x, y: y, dirIdx: dirIdx)
        if seen.contains(state) {
            return true
        }
        seen.insert(state)
        
        let (dirX, dirY) = dirs[dirIdx]
        let nx = x + dirX
        let ny = y + dirY
        
        if nx < 0 || nx >= w || ny < 0 || ny >= h {
            return false
        }
        
        if grid[ny][nx] == "#" {
            dirIdx = (dirIdx + 1) % 4
            continue
        }
        
        x = nx
        y = ny
    }
    
    return false
}

func findLoopPositions(inputString: String) -> Int {
    var grid = inputString.trimmingCharacters(in: .whitespacesAndNewlines)
        .components(separatedBy: "\n")
        .filter { !$0.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty }
        .map { Array($0) }
    
    let h = grid.count
    let w = grid[0].count
    
    var startX: Int!
    var startY: Int!
    var startDir: Int = 0
    
    for i in 0..<h {
        for j in 0..<w {
            switch grid[i][j] {
            case "^":
                startX = j
                startY = i
                startDir = 0
            case ">":
                startX = j
                startY = i
                startDir = 1
            case "v":
                startX = j
                startY = i
                startDir = 2
            case "<":
                startX = j
                startY = i
                startDir = 3
            default:
                break
            }
        }
    }
    
    grid[startY][startX] = "."
    
    var canLoop = 0
    for y in 0..<h {
        for x in 0..<w {
            if x == startX && y == startY {
                continue
            }
            if grid[y][x] != "." {
                continue
            }
            
            grid[y][x] = "#"
            if loops(grid: &grid, startX: startX, startY: startY, startDir: startDir) {
                canLoop += 1
            }
            grid[y][x] = "."
        }
    }
    
    return canLoop
}

if let inputString = try? String(contentsOfFile: "input.txt", encoding: .utf8) {
    print(findLoopPositions(inputString: inputString))
} else {
    print("Could not read input file")
}
