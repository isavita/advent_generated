
import Foundation

// Function to read the input from a file and return a 2D array of integers
func readInput(from filename: String) -> [[Int]]? {
    do {
        let contents = try String(contentsOfFile: filename)
        let rows = contents.split(separator: "\n").map { String($0) }
        return rows.map { $0.compactMap { Int(String($0)) } }
    } catch {
        print("Error reading file: \(error)")
        return nil
    }
}

// Function to check if a tree is visible
func isVisible(tree: Int, at row: Int, col: Int, in grid: [[Int]]) -> Bool {
    let directions = [
        (0, 1),   // right
        (1, 0),   // down
        (0, -1),  // left
        (-1, 0)   // up
    ]
    
    for (dx, dy) in directions {
        var x = row + dx
        var y = col + dy
        var blocked = false
        
        while x >= 0 && x < grid.count && y >= 0 && y < grid[0].count {
            if grid[x][y] >= tree {
                blocked = true
                break
            }
            x += dx
            y += dy
        }
        
        if !blocked {
            return true
        }
    }
    
    return false
}

// Function to count visible trees
func countVisibleTrees(in grid: [[Int]]) -> Int {
    let rows = grid.count
    let cols = grid[0].count
    var visibleCount = 0
    
    // Count edge trees
    visibleCount += (2 * rows) + (2 * (cols - 2)) // Count all edge trees
    
    // Check interior trees
    for row in 1..<rows-1 {
        for col in 1..<cols-1 {
            if isVisible(tree: grid[row][col], at: row, col: col, in: grid) {
                visibleCount += 1
            }
        }
    }
    
    return visibleCount
}

// Main execution
if let grid = readInput(from: "input.txt") {
    let visibleTrees = countVisibleTrees(in: grid)
    print("Total visible trees: \(visibleTrees)")
} else {
    print("Failed to read the input.")
}
