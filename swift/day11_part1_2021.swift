
import Foundation

func readInput(filename: String) -> [[Int]] {
    let fileURL = URL(fileURLWithPath: filename)
    let contents = try! String(contentsOf: fileURL)
    
    var grid = [[Int]]()
    let lines = contents.components(separatedBy: "\n")
    for line in lines {
        var row = [Int]()
        for char in line {
            if let num = Int(String(char)) {
                row.append(num)
            }
        }
        grid.append(row)
    }
    
    return grid
}

func simulateStep(grid: inout [[Int]]) -> Int {
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
                flashes += flash(grid: &grid, x: x, y: y, flashed: &flashed)
            }
        }
    }
    
    for coords in flashed {
        grid[coords[1]][coords[0]] = 0
    }
    
    return flashes
}

func flash(grid: inout [[Int]], x: Int, y: Int, flashed: inout Set<[Int]>) -> Int {
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
                flashes += flash(grid: &grid, x: newX, y: newY, flashed: &flashed)
            }
        }
    }
    
    return flashes
}

var grid = readInput(filename: "input.txt") // Changed 'let' to 'var' to make grid mutable
var totalFlashes = 0
for _ in 0..<100 {
    totalFlashes += simulateStep(grid: &grid)
}

print(totalFlashes)
