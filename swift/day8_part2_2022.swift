
import Foundation

// Function to read the input from a file and return the grid of tree heights
func readInput(from file: String) -> [[Int]] {
    do {
        let content = try String(contentsOfFile: file)
        return content.split(separator: "\n").map { line in
            line.compactMap { Int(String($0)) }
        }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

// Function to count visible trees
func countVisibleTrees(grid: [[Int]]) -> Int {
    let rows = grid.count
    let cols = grid[0].count
    var visibleCount = 0
    
    // Check visibility for each tree
    for r in 0..<rows {
        for c in 0..<cols {
            let height = grid[r][c]
            var isVisible = false
            
            // Check left
            if (0..<c).allSatisfy({ grid[r][$0] < height }) { isVisible = true }
            // Check right
            if (c+1..<cols).allSatisfy({ grid[r][$0] < height }) { isVisible = true }
            // Check up
            if (0..<r).allSatisfy({ grid[$0][c] < height }) { isVisible = true }
            // Check down
            if (r+1..<rows).allSatisfy({ grid[$0][c] < height }) { isVisible = true }
            
            if isVisible {
                visibleCount += 1
            }
        }
    }
    
    // Add the edge trees which are always visible
    visibleCount += (2 * rows) + (2 * (cols - 2)) // Top and bottom rows + left and right columns excluding corners
    return visibleCount
}

// Function to calculate the scenic score for each tree
func calculateScenicScores(grid: [[Int]]) -> Int {
    let rows = grid.count
    let cols = grid[0].count
    var maxScenicScore = 0
    
    for r in 0..<rows {
        for c in 0..<cols {
            let height = grid[r][c]
            var up = 0, down = 0, left = 0, right = 0
            
            // Count trees in each direction until blocked
            // Up
            for i in stride(from: r - 1, through: 0, by: -1) {
                up += 1
                if grid[i][c] >= height { break }
            }
            // Down
            for i in r + 1..<rows {
                down += 1
                if grid[i][c] >= height { break }
            }
            // Left
            for i in stride(from: c - 1, through: 0, by: -1) {
                left += 1
                if grid[r][i] >= height { break }
            }
            // Right
            for i in c + 1..<cols {
                right += 1
                if grid[r][i] >= height { break }
            }
            
            let scenicScore = up * down * left * right
            maxScenicScore = max(maxScenicScore, scenicScore)
        }
    }
    
    return maxScenicScore
}

// Main execution
let grid = readInput(from: "input.txt")
let visibleTrees = countVisibleTrees(grid: grid)
let highestScenicScore = calculateScenicScores(grid: grid)

print("Total visible trees: \(visibleTrees)")
print("Highest scenic score: \(highestScenicScore)")
