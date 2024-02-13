
import Foundation

func readInput(_ filename: String) -> [[Int]] {
    let fileURL = URL(fileURLWithPath: filename)
    let content = try! String(contentsOf: fileURL)
    var grid = [[Int]]()
    
    content.enumerateLines { line, _ in
        let row = line.map { Int(String($0)) ?? 0 }
        grid.append(row)
    }
    
    return grid
}

func simulateStep(_ grid: inout [[Int]]) -> Int {
    var flashes = 0
    var flashed = Set<[Int]>()
    
    for y in 0..<grid.count {
        for x in 0..<grid[y].count {
            grid[y][x] += 1
        }
    }
    
    for y in 0..<grid.count {
        for x in 0..<grid[y].count {
            if grid[y][x] > 9 {
                flashes += flash(&grid, x, y, &flashed)
            }
        }
    }
    
    for coords in flashed {
        grid[coords[1]][coords[0]] = 0
    }
    
    return flashes
}

func flash(_ grid: inout [[Int]], _ x: Int, _ y: Int, _ flashed: inout Set<[Int]>) -> Int {
    if flashed.contains([x, y]) {
        return 0
    }
    
    flashed.insert([x, y])
    var flashes = 1
    let directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
    
    for dir in directions {
        let newX = x + dir[0]
        let newY = y + dir[1]
        if newX >= 0 && newX < grid[0].count && newY >= 0 && newY < grid.count {
            grid[newY][newX] += 1
            if grid[newY][newX] > 9 {
                flashes += flash(&grid, newX, newY, &flashed)
            }
        }
    }
    
    return flashes
}

var grid = readInput("input.txt")
var step = 0

while true {
    step += 1
    let flashes = simulateStep(&grid)
    if flashes == 100 {
        break
    }
}

print(step)
